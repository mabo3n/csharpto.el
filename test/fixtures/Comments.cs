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

        // I'm a comment above function
        public MyEntity(string name)
        {

            // I'm a comment inside a function
            Id = Guid.NewGuid();
            logs = new List<LogEntry>() { };
            Name = name;
            // I'm a comment inside a function
        }

        int OneLiner() => 1 + 2 + 3 + 4 + 5; //I'm a comment after an one-line function


        void Log(                                           // Who
            DateTime timestamp, LogLevel? level = default   // comment
        )                                                   // like this
            => throw new NotImplementedException();

        // I'm a comment between functions

        /// <summary>
        /// Return the sum of a incremented by 1
        /// and b incremented by 1.
        /// </summary>
        /// <param name="a">the first param</param>
        /// <param name="b">the second param</param>
        public int SomeMethod(int a, int b) {
            a++;

            b++;

            return a + b;

        } // end of SomeMethod

        /* This is a multi line comment
         * it can't have blank lines
         */
        public int AnotherMethod(
            int x, int y
        ) { /* comment */
            x++;

            y++;

            return x + y;

        }

        // Comments
        public IEnumerable<char> GetSequence() //in
        /* every */
            => this//line
            .GetHashCode() // of
            .ToString();//function
        /* end of file (this shouldn't be matched)*/
    }
}
