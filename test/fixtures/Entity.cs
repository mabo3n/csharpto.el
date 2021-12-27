using System.Collections.ObjectModel;
using MyNamespace.Domain.Enums;
using MyNamespace.Domain.ValueObjects;

namespace MyNamespace.Domain.Entities
{
    public class MyEntity
    {
        protected Guid Id { get; private set; }
        public string Name { get; private set; }

        private List<LogEntry> logs;

        public ReadOnlyCollection<LogEntry> Logs
            => throw new NotImplementedException();

        public MyEntity(string name)
        {
            Id = Guid.NewGuid();
            logs = new List<LogEntry>() { };
            Name = name;
        }

        int OneLiner() => 1 + 2 + 3 + 4 + 5;


        void Log(
            DateTime timestamp, LogLevel? level = default
        )
            => throw new NotImplementedException();

        public int SomeMethod(int a, int b) {
            a++;
            b++;
            return a + b;
        }

        public IEnumerable<char> GetSequence()
            => this
            .GetHashCode()
            .ToString();

    }
}
