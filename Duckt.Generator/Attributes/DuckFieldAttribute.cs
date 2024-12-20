namespace Duckt.Generator.Attributes;

[AttributeUsage(AttributeTargets.Property)]
public class DuckFieldAttribute : Attribute
{
    private string _fieldName;

    public DuckFieldAttribute(string fieldName = null)
    {
        _fieldName = fieldName;
    }

    public string GetName(string propertyName)
    {
        return _fieldName ??= propertyName;
    }
}
