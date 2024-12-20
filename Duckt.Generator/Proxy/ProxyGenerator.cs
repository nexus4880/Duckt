namespace Duckt.Generator.Proxy;

public abstract class ProxyGenerator
{
	public static ProxyGenerator Default = new DynamicTypingProxyGenerator();

	public abstract object CreateProxy(object instance, Type toType);
}
