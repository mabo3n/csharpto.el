
namespace Company.Generics
{
    public class Generics
    {
        T Get<T>() => default(T);

        T Set<T, U>(ref U u) where
            T: new(), U {
            u = default(T);
        }
    }
}
