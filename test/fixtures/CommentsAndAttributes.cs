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

        [Attr] public ReadOnlyCollection<LogEntry> Logs
            => throw new NotImplementedException();

        // This is a comment
        // before attributes
        [Attr]
        [Attr]
        public MyEntity(string name)
        {
            Id = Guid.NewGuid();
            logs = new List<LogEntry>() { };
            Name = name;
        }

        [Attr] int OneLiner() => 1 + 2 + 3 + 4 + 5;     //Comment

        [Attr]
        [Attr]
        // Comment
        // after attributes
        void Log(
            DateTime timestamp, LogLevel? level = default
        )
            => throw new NotImplementedException();

        // I'm a comment between functions

        // Comment before
        [Attr]
        [Attr]
        // and after attributes
        public int SomeMethod(int a, int b) {
            a++;

            b++;

            return a + b;

        } // end of SomeMethod

        /*
         * I'm a comment between functions
         */

        /* Block comment before */
        [Attr]
        [Attr]
        /* and after attributes */
        public int AnotherMethod(
            int x, int y
        ) { /* comment */
            x++;

            y++;

            return x + y;

        }

        /* Comment before */
        [Attr("Wow")]
        // Comment before attribute
        [Attr]
        [Attribute(
            typeof(MyEntity))  /* Comment by side */
        ]
        /* Comment after attributes */
        public IEnumerable<char> GetSequence() //Comment madness
        /* Help */
            => this//Wow
            .GetHashCode() // yes
            .ToString();//bye
        // end of file (this shouldn't be matched)
    }
}
