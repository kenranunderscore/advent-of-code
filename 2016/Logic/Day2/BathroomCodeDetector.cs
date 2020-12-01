namespace Logic.Day2
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Numerics;

    public class BathroomCodeDetector
    {
        public static readonly IReadOnlyDictionary<string, Vector2> NumpadMapping
            = new Dictionary<string, Vector2>
            {
                { "1", new Vector2(-1, 1) },
                { "2", new Vector2(0, 1) },
                { "3", new Vector2(1, 1) },
                { "4", new Vector2(-1, 0) },
                { "5", new Vector2(0, 0) },
                { "6", new Vector2(1, 0) },
                { "7", new Vector2(-1, -1) },
                { "8", new Vector2(0, -1) },
                { "9", new Vector2(1, -1) }
            };

        public static readonly IReadOnlyDictionary<string, Vector2> Part2Mapping
            = new Dictionary<string, Vector2>
            {
                { "1", new Vector2(0, 2) },
                { "2", new Vector2(-1, 1) },
                { "3", new Vector2(0, 1) },
                { "4", new Vector2(1, 1) },
                { "5", new Vector2(-2, 0) },
                { "6", new Vector2(-1, 0) },
                { "7", new Vector2(0, 0) },
                { "8", new Vector2(1, 0) },
                { "9", new Vector2(2, 0) },
                { "A", new Vector2(-1, -1) },
                { "B", new Vector2(0, -1) },
                { "C", new Vector2(1, -1) },
                { "D", new Vector2(0, -2) }
            };

        private static readonly IReadOnlyDictionary<char, Vector2> DirectionMapping
            = new Dictionary<char, Vector2>
            {
                { 'U', new Vector2(0, 1) },
                { 'D', new Vector2(0, -1) },
                { 'L', new Vector2(-1, 0) },
                { 'R', new Vector2(1, 0) }
            };

        public string DecipherCode(IEnumerable<string> input, IReadOnlyDictionary<string, Vector2> mapping)
        {
            string currentCode = string.Empty;
            foreach (string line in input)
            {
                currentCode += CalculateCipherFromLine(line, string.IsNullOrEmpty(currentCode) ? "5" : currentCode, mapping);
            }

            return currentCode;
        }

        private static string CalculateCipherFromLine(string line, string currentCode, IReadOnlyDictionary<string, Vector2> mapping)
        {
            var currentPosition = mapping[currentCode.Substring(currentCode.Length - 1)];

            for (int i = 0; i < line.Length; ++i)
            {
                var direction = DirectionMapping[line[i]];
                currentPosition = CalculateNextPositionWithinBoundaries(currentPosition, direction, mapping);
            }

            return mapping.First(x => x.Value == currentPosition).Key.ToString();
        }

        private static Vector2 CalculateNextPositionWithinBoundaries(Vector2 position, Vector2 direction, IReadOnlyDictionary<string, Vector2> mapping)
        {
            var newPosition = position + direction;
            return mapping.Values.Contains(newPosition) ? newPosition : position;
        }
    }
}