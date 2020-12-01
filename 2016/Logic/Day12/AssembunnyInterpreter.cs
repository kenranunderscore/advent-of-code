namespace Logic.Day12
{
    using System.Collections.Generic;
    using System.Text.RegularExpressions;

    public class AssembunnyInterpreter
    {
        private static readonly Regex reg = new Regex(
            @"(cpy ((?<cpy_source_r>[a-d])|(?<cpy_source_n>\-?\d+)) (?<cpy_r>[a-d])|inc (?<inc_r>[a-d])|dec (?<dec_r>[a-d])|jnz ((?<jnz_r>[a-d])|(?<jnz_v>\-?\d+)) (?<jnz_n>\-?\d+))");

        private readonly Dictionary<string, int> registers
            = new Dictionary<string, int>
            {
                { "a", 0 },
                { "b", 0 },
                { "c", 0 },
                { "d", 0 },
            };

        public IDictionary<string, int> Registers => registers;

        public IReadOnlyList<string> Input { get; set; }

        public void Run()
        {
            for (int i = 0; i < Input.Count; i++)
            {
                string line = Input[i];
                var match = reg.Match(line);

                if (match.Groups["inc_r"].Success)
                {
                    ++registers[match.Groups["inc_r"].Value];
                    continue;
                }

                if (match.Groups["dec_r"].Success)
                {
                    --registers[match.Groups["dec_r"].Value];
                    continue;
                }

                if (match.Groups["cpy_r"].Success)
                {
                    string target = match.Groups["cpy_r"].Value;
                    if (match.Groups["cpy_source_r"].Success)
                    {
                        registers[target] = registers[match.Groups["cpy_source_r"].Value];
                    }
                    else
                    {
                        registers[target] = int.Parse(match.Groups["cpy_source_n"].Value);
                    }

                    continue;
                }

                int jumpLength = int.Parse(match.Groups["jnz_n"].Value);
                int val = match.Groups["jnz_r"].Success
                    ? registers[match.Groups["jnz_r"].Value]
                    : int.Parse(match.Groups["jnz_v"].Value);
                if (val != 0)
                {
                    i += jumpLength - 1;
                }
            }
        }
    }
}