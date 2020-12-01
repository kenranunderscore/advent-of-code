namespace Logic.Day8
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Linq;
    using System.Text;
    using System.Text.RegularExpressions;

    public class ScreenOperator
    {
        private static readonly Regex rectMatcher = new Regex(@"rect (?<width>\d+)x(?<height>\d+)");
        private static readonly Regex rotateColumnMatcher = new Regex(@"rotate column x=(?<column>\d+) by (?<shift>\d+)");
        private static readonly Regex rotateRowMatcher = new Regex(@"rotate row y=(?<row>\d+) by (?<shift>\d+)");

        public int LitPixelCount(IEnumerable<string> operations)
        {
            var screen = new Screen();
            foreach (string s in operations)
            {
                var match = rectMatcher.Match(s);
                if (match.Success)
                {
                    screen.Rect(int.Parse(match.Groups["width"].Value), int.Parse(match.Groups["height"].Value));
                    continue;
                }

                match = rotateColumnMatcher.Match(s);
                if (match.Success)
                {
                    screen.RotateColumn(int.Parse(match.Groups["column"].Value), int.Parse(match.Groups["shift"].Value));
                    continue;
                }

                match = rotateRowMatcher.Match(s);
                if (match.Success)
                {
                    screen.RotateRow(int.Parse(match.Groups["row"].Value), int.Parse(match.Groups["shift"].Value));
                }
            }

            return screen.NumberOfLitPixels;
        }
    }
}