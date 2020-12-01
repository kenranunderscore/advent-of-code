namespace Logic.Day6
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;

    public class RepetitionCodeDecipherer
    {
        public string DeciperMessagePart1(IEnumerable<string> input) => DecipherMessage(input, MostFrequentCharacter);

        public string DeciperMessagePart2(IEnumerable<string> input) => DecipherMessage(input, LeastFrequentCharacter);

        private string DecipherMessage(IEnumerable<string> input, Func<IEnumerable<char>, char> columnCharacterSelector)
        {
            int columnCount = input.First().Length;
            StringBuilder sb = new StringBuilder(columnCount);
            for (int i = 0; i < columnCount; i++)
            {
                var column = input.Select(s => s[i]);
                var mostFrequentCharacter = columnCharacterSelector(column);
                sb.Append(mostFrequentCharacter);
            }

            return sb.ToString();
        }

        private static char MostFrequentCharacter(IEnumerable<char> column) =>
            column
                .GroupBy(c => c, (c, g) => (c, g.Count()))
                .OrderByDescending(c => c.Item2)
                .First()
                .c;

        private static char LeastFrequentCharacter(IEnumerable<char> column) =>
            column
                .GroupBy(c => c, (c, g) => (c, g.Count()))
                .OrderBy(c => c.Item2)
                .First()
                .c;
    }
}