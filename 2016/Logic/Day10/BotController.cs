namespace Logic.Day10
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class BotController
    {
        Regex valueRegex = new Regex(@"value (?<val>\d+) goes to bot (?<id>\d+)");
        Regex botRegex = new Regex(@"bot (?<id>\d+) gives low to (?<lowTargetType>(bot|output)) (?<lowId>\d+) and high to (?<highTargetType>(bot|output)) (?<highId>\d+)");

        private readonly Dictionary<int, Action<int>> bots = new Dictionary<int, Action<int>>();
        private readonly int[] outputs = new int[21];

        public int FindBotHandlingNumbers(IEnumerable<string> orders, int min, int max)
        {
            int targetId = -1;

            foreach (var match in orders.Select(l => botRegex.Match(l)).Where(m => m.Success))
            {
                int id = int.Parse(match.Groups["id"].Value);
                List<int> numbers = new List<int>();
                bots[id] = num =>
                {
                    numbers.Add(num);
                    if (numbers.Count == 2)
                    {
                        int low = numbers.Min();
                        int high = numbers.Max();

                        if (low == min && high == max)
                        {
                            targetId = id;
                        }

                        var lowTargetType = match.Groups["lowTargetType"].Value;
                        int lowId = int.Parse(match.Groups["lowId"].Value);
                        var highTargetType = match.Groups["highTargetType"].Value;
                        int highTargetId = int.Parse(match.Groups["highId"].Value);

                        if (lowTargetType == "output")
                        {
                            outputs[lowId] = low;
                        }
                        else
                        {
                            bots[lowId](low);
                        }

                        if (highTargetType == "output")
                        {
                            outputs[highTargetId] = high;
                        }
                        else
                        {
                            bots[highTargetId](high);
                        }
                    }
                };
            }

            foreach (var match in orders.Select(l => valueRegex.Match(l)).Where(m => m.Success))
            {
                int id = int.Parse(match.Groups["id"].Value);
                int value = int.Parse(match.Groups["val"].Value);
                bots[id](value);
            }

            return targetId;
        }

        public int Part2 => outputs[0] * outputs[1] * outputs[2];
    }
}