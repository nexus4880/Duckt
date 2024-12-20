using Duckt.Generator.Proxy;

namespace Duckt.Generator.Extensions;

public static class DuckTypingExtensions
{
    public static T Duckt<T>(this object source)
    {
        return source.Duckt<T>(ProxyGenerator.Default);
    }

    public static T Duckt<T>(this object source, ProxyGenerator proxy)
    {
        if (source == null)
        {
            throw new ArgumentNullException(nameof(source));
        }

        if (proxy == null)
        {
            throw new ArgumentNullException(nameof(proxy));
		}

		return (T)proxy.CreateProxy(source, typeof(T));
    }
}
