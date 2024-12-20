using System.Reflection;
using System.Reflection.Emit;
using Duckt.Generator.Attributes;

namespace Duckt.Generator.Proxy;

internal class DynamicTypingProxyGenerator : ProxyGenerator
{
	public override object CreateProxy(object instance, Type interfaceType)
	{
		var sourceType = instance.GetType();

		if (!interfaceType.IsInterface)
		{
			throw new ArgumentException(
				$"{interfaceType.Name} must be an interface", nameof(interfaceType)
			);
		}

		var proxyTypeName = $"DuckTypedProxy_{sourceType.Name}_{interfaceType.Name}";
		var interfaceMethods = interfaceType.GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

		foreach (var interfaceMethod in interfaceMethods)
		{
			var matchingMethod = sourceType.GetMethod(interfaceMethod.Name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

			if (matchingMethod == null)
			{
				var propertyName = interfaceMethod.Name.Replace("get_", null).Replace("set_", null);
				var property = interfaceType.GetProperty(propertyName);

				if (property == null)
				{
					throw new InvalidOperationException(
						$"Could not find matching method {interfaceMethod.Name} in {sourceType.Name}"
					);
				}

				var duckFieldAttribute = property.GetCustomAttribute<DuckFieldAttribute>();
				if (duckFieldAttribute == null)
				{
					throw new InvalidOperationException(
						$"Could not find matching method {interfaceMethod.Name} in {sourceType.Name}"
					);
				}

				var fieldInfo = sourceType.GetField(duckFieldAttribute.GetName(propertyName), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
				if (fieldInfo == null)
				{
					throw new InvalidOperationException(
						$"Missing desired field for {propertyName}: {duckFieldAttribute.GetName(propertyName)} on {sourceType.Name}"
					);
				}

				if (property.PropertyType != fieldInfo.FieldType)
				{
					throw new InvalidOperationException(
						$"Type mismatch between {interfaceType.Name}.{propertyName} and {duckFieldAttribute.GetName(propertyName)}"
					);
				}

				continue;
			}

			if (!interfaceMethod.ReturnType.IsAssignableFrom(matchingMethod.ReturnType))
			{
				throw new InvalidOperationException(
					$"Method {interfaceMethod.Name} return type mismatch. " +
					$"Expected {interfaceMethod.ReturnType}, " +
					$"but found {matchingMethod.ReturnType}"
				);
			}

			var interfaceParameters = interfaceMethod.GetParameters();
			var matchingParameters = matchingMethod.GetParameters();
			if (interfaceParameters.Length != matchingParameters.Length)
			{
				throw new InvalidOperationException(
					$"Parameter mismatch on {interfaceMethod.Name} for {sourceType}"
				);
			}

			for (var i = 0; i < interfaceParameters.Length; i++)
			{
				if (interfaceParameters[i].ParameterType != matchingParameters[i].ParameterType)
				{
					throw new InvalidOperationException(
						$"Method {interfaceMethod.Name} is not compatible"
					);
				}
			}
		}

		var assemblyName = new AssemblyName(Guid.NewGuid().ToString());
		var assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(
			assemblyName,
			AssemblyBuilderAccess.Run
		);

		var moduleBuilder = assemblyBuilder.DefineDynamicModule("DuckTypingModule");
		var typeBuilder = moduleBuilder.DefineType(
			proxyTypeName,
			TypeAttributes.Public |
			TypeAttributes.Class |
			TypeAttributes.AutoClass |
			TypeAttributes.AnsiClass |
			TypeAttributes.BeforeFieldInit |
			TypeAttributes.AutoLayout,
			null,
			[interfaceType]
		);

		var sourceField = typeBuilder.DefineField(
			"_source",
			sourceType,
			FieldAttributes.Private |
			FieldAttributes.InitOnly
		);

		var constructorBuilder = typeBuilder.DefineConstructor(
			MethodAttributes.Public |
			MethodAttributes.SpecialName |
			MethodAttributes.RTSpecialName,
			CallingConventions.Standard,
			[sourceType]
		);

		var constructorIL = constructorBuilder.GetILGenerator();

		constructorIL.Emit(OpCodes.Ldarg_0);
		constructorIL.Emit(OpCodes.Call, typeof(object).GetConstructor(Type.EmptyTypes));
		constructorIL.Emit(OpCodes.Ldarg_0);
		constructorIL.Emit(OpCodes.Ldarg_1);
		constructorIL.Emit(OpCodes.Stfld, sourceField);
		constructorIL.Emit(OpCodes.Ret);

		foreach (var interfaceMethod in interfaceMethods)
		{
			var matchingMethod = sourceType.GetMethod(interfaceMethod.Name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

			var methodBuilder = typeBuilder.DefineMethod(
				interfaceMethod.Name,
				MethodAttributes.Public |
				MethodAttributes.SpecialName |
				MethodAttributes.Virtual |
				MethodAttributes.HideBySig |
				MethodAttributes.NewSlot |
				MethodAttributes.Final,
				interfaceMethod.ReturnType,
				interfaceMethod.GetParameters().Select(p => p.ParameterType).ToArray()
			);

			var methodIL = methodBuilder.GetILGenerator();

			if (matchingMethod != null)
			{
				if (matchingMethod.IsPrivate)
				{
					GeneratePrivateMethodInvoker(methodIL, sourceField, matchingMethod, interfaceMethod);
				}
				else
				{
					methodIL.Emit(OpCodes.Ldarg_0);
					methodIL.Emit(OpCodes.Ldfld, sourceField);

					var parameters = interfaceMethod.GetParameters();

					for (int i = 0; i < parameters.Length; i++)
					{
						methodIL.Emit(OpCodes.Ldarg, i + 1);
					}

					methodIL.Emit(OpCodes.Callvirt, matchingMethod);
				}
			}
			else
			{
				if (interfaceMethod.Name.StartsWith("get_"))
				{
					var propertyName = interfaceMethod.Name.Replace("get_", null);
					var property = interfaceType.GetProperty(propertyName);
					var duckFieldAttribute = property.GetCustomAttribute<DuckFieldAttribute>();
					var field = sourceType.GetField(duckFieldAttribute.GetName(propertyName), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

					GenerateFieldPropertyGetter(
						methodIL,
						sourceField,
						field,
						interfaceMethod.ReturnType
					);
				}
				else
				{
					var propertyName = interfaceMethod.Name.Replace("set_", null);
					var property = interfaceType.GetProperty(propertyName);
					var duckFieldAttribute = property.GetCustomAttribute<DuckFieldAttribute>();
					var field = sourceType.GetField(duckFieldAttribute.GetName(propertyName), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

					GenerateFieldPropertySetter(
						methodIL,
						sourceField,
						field
					);
				}
			}

			methodIL.Emit(OpCodes.Ret);
		}

		return Activator.CreateInstance(typeBuilder.CreateTypeInfo(), instance);
	}

    private static void GeneratePrivateMethodInvoker(
    ILGenerator methodIL,
    FieldInfo sourceField,
    MethodInfo matchingMethod,
    MethodInfo interfaceMethod)
    {
        // SourceField localSourceInstance;
        LocalBuilder localSourceInstance = methodIL.DeclareLocal(sourceField.FieldType);

        // MethodInfo localMethodInfo;
        LocalBuilder localMethodInfo = methodIL.DeclareLocal(typeof(MethodInfo));

        // object[] localParameters;
        LocalBuilder localParameters = methodIL.DeclareLocal(typeof(object[]));

        // this.localSourceInstance = sourceField
        methodIL.Emit(OpCodes.Ldarg_0);
        methodIL.Emit(OpCodes.Ldfld, sourceField);
        methodIL.Emit(OpCodes.Stloc, localSourceInstance);

        // localMethodInfo = this.localSourceInstance.GetType()
        // .GetMethod(matchingMethod.Name, BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public)
        methodIL.Emit(OpCodes.Ldloc, localSourceInstance);
        methodIL.Emit(OpCodes.Call, typeof(object).GetMethod("GetType"));
        methodIL.Emit(OpCodes.Ldstr, matchingMethod.Name);
        methodIL.Emit(OpCodes.Ldc_I4, (int)(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public));
        methodIL.Emit(OpCodes.Callvirt, typeof(Type).GetMethod("GetMethod", [typeof(string), typeof(BindingFlags)]));
        methodIL.Emit(OpCodes.Stloc, localMethodInfo);

        // localParameters = new object[parameters.Length];
        ParameterInfo[] parameters = interfaceMethod.GetParameters();
        methodIL.Emit(OpCodes.Ldc_I4, parameters.Length);
        methodIL.Emit(OpCodes.Newarr, typeof(object));
        methodIL.Emit(OpCodes.Stloc, localParameters);

        for (int i = 0; i < parameters.Length; i++)
        {
            // localParameters[i] = args[i + 1];
            methodIL.Emit(OpCodes.Ldloc, localParameters);
            methodIL.Emit(OpCodes.Ldc_I4, i);
            methodIL.Emit(OpCodes.Ldarg, i + 1);

            if (parameters[i].ParameterType.IsValueType)
            {
                methodIL.Emit(OpCodes.Box, parameters[i].ParameterType);
            }

            methodIL.Emit(OpCodes.Stelem_Ref);
        }

        // localMethodInfo.Invoke(localSourceInstance, localParameters)
        methodIL.Emit(OpCodes.Ldloc, localMethodInfo);
        methodIL.Emit(OpCodes.Ldloc, localSourceInstance);
        methodIL.Emit(OpCodes.Ldloc, localParameters);
        methodIL.Emit(OpCodes.Callvirt, typeof(MethodInfo).GetMethod("Invoke", [typeof(object), typeof(object[])]));

        if (interfaceMethod.ReturnType == typeof(void))
        {
            methodIL.Emit(OpCodes.Pop);
        }
        else if (interfaceMethod.ReturnType.IsValueType)
        {
            methodIL.Emit(OpCodes.Unbox_Any, interfaceMethod.ReturnType);
        }
        else
        {
            methodIL.Emit(OpCodes.Castclass, interfaceMethod.ReturnType);
        }
    }

    private static void GenerateFieldPropertyGetter(
    ILGenerator methodIL,
    FieldInfo sourceField,
    FieldInfo privateField,
    Type returnType)
    {
        // SourceField localSourceInstance;
        LocalBuilder localSourceInstance = methodIL.DeclareLocal(sourceField.FieldType);

        // FieldInfo localFieldInfo;
        LocalBuilder localFieldInfo = methodIL.DeclareLocal(typeof(FieldInfo));

        // object localFieldValue;
        LocalBuilder localFieldValue = methodIL.DeclareLocal(typeof(object));

        // this.localSourceInstance = sourceField;
        methodIL.Emit(OpCodes.Ldarg_0);
        methodIL.Emit(OpCodes.Ldfld, sourceField);
        methodIL.Emit(OpCodes.Stloc, localSourceInstance);


        // localFieldInfo = localSourceInstance.GetType()
        // .GetField(privateField.Name, BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
        methodIL.Emit(OpCodes.Ldloc, localSourceInstance);
        methodIL.Emit(OpCodes.Call, typeof(object).GetMethod("GetType"));
        methodIL.Emit(OpCodes.Ldstr, privateField.Name);
        methodIL.Emit(OpCodes.Ldc_I4, (int)(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public));
        methodIL.Emit(OpCodes.Callvirt, typeof(Type).GetMethod("GetField", [typeof(string), typeof(BindingFlags)]));
        methodIL.Emit(OpCodes.Stloc, localFieldInfo);

        // localFieldValue = localFieldInfo.GetValue(localSourceInstance);
        methodIL.Emit(OpCodes.Ldloc, localFieldInfo);
        methodIL.Emit(OpCodes.Ldloc, localSourceInstance);
        methodIL.Emit(OpCodes.Callvirt, typeof(FieldInfo).GetMethod("GetValue", [typeof(object)]));
        methodIL.Emit(OpCodes.Stloc, localFieldValue);

        // Load return value
        methodIL.Emit(OpCodes.Ldloc, localFieldValue);

        if (returnType.IsValueType)
        {
            methodIL.Emit(OpCodes.Unbox_Any, returnType);
        }
        else
        {
            methodIL.Emit(OpCodes.Castclass, returnType);
        }
    }

    private static void GenerateFieldPropertySetter(
        ILGenerator methodIL,
        FieldInfo sourceField,
        FieldInfo privateField)
    {
        // SourceField localSourceInstance;
        LocalBuilder localSourceInstance = methodIL.DeclareLocal(sourceField.FieldType);

        // FieldInfo localFieldInfo;
        LocalBuilder localFieldInfo = methodIL.DeclareLocal(typeof(FieldInfo));

        // localSourceInstance = this.sourceField;
        methodIL.Emit(OpCodes.Ldarg_0);
        methodIL.Emit(OpCodes.Ldfld, sourceField);
        methodIL.Emit(OpCodes.Stloc, localSourceInstance);

        // localFieldInfo = localSourceInstance.GetType()
        // .GetField(privateField.Name, BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
        methodIL.Emit(OpCodes.Ldloc, localSourceInstance);
        methodIL.Emit(OpCodes.Call, typeof(object).GetMethod("GetType"));
        methodIL.Emit(OpCodes.Ldstr, privateField.Name);
        methodIL.Emit(OpCodes.Ldc_I4, (int)(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public));
        methodIL.Emit(OpCodes.Callvirt, typeof(Type).GetMethod("GetField", [typeof(string), typeof(BindingFlags)]));
        methodIL.Emit(OpCodes.Stloc, localFieldInfo);

        // {
        methodIL.Emit(OpCodes.Ldloc, localFieldInfo);
        methodIL.Emit(OpCodes.Ldloc, localSourceInstance);
        methodIL.Emit(OpCodes.Ldarg_1);

        if (privateField.FieldType.IsValueType)
        {
            methodIL.Emit(OpCodes.Box, privateField.FieldType);
        }

        // localFieldInfo.SetValue(localSourceInstance, args[1]);
        methodIL.Emit(OpCodes.Callvirt, typeof(FieldInfo).GetMethod("SetValue", [typeof(object), typeof(object)]));
        // }
    }
}