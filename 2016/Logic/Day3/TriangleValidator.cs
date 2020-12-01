namespace Logic.Day3
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class TriangleValidator
    {
        public int NumberOfValidTriangles(IEnumerable<string> input) => input
            .Select(l => l.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries))
            .Select(l => l.Select(int.Parse).ToArray())
            .Select(l => new Triangle
            {
                a = l[0],
                b = l[1],
                c = l[2]
            })
            .Count(t => t.IsValid);

        public int NumberOfValidTrianglesInColumns(IEnumerable<string> input) =>
            NoOfValidTrianglesInColumn(GetColumn(input, 0))
                + NoOfValidTrianglesInColumn(GetColumn(input, 1))
                + NoOfValidTrianglesInColumn(GetColumn(input, 2));

        private static int NoOfValidTrianglesInColumn(IList<int> col)
        {
            int count = 0;
            for (int i = 0; i < col.Count; i += 3)
            {
                Triangle t = new Triangle { a = col[i], b = col[i + 1], c = col[i + 2] };
                if (t.IsValid)
                    ++count;
            }

            return count;
        }

        private static IList<int> GetColumn(IEnumerable<string> lines, int number)
        {
            var intLines =
                lines
                    .Select(l => l.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries))
                    .Select(l => l.Select(int.Parse).ToArray());
            return intLines.Select(x => x[number]).ToList();
        }

        private struct Triangle
        {
            public int a, b, c;

            public bool IsValid =>
                a + b > c
                && a + c > b
                && b + c > a;
        }
    }
}