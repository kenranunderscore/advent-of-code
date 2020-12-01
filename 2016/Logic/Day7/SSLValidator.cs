namespace Logic.Day7
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class SSLValidator
    {
        private static readonly Regex bracketContentRegex = new Regex(@"\[(\w+)\]", RegexOptions.IgnoreCase);

        public bool SupportsSSL(string sequence)
        {
            var abas = ExtractABAs(sequence);

            foreach (string aba in abas)
            {
                string bab = new string(new[] { aba[1], aba[0], aba[1] });
                foreach (var match in bracketContentRegex.Matches(sequence).Cast<Match>())
                {
                    if (match.Groups[1].Value.Contains(bab))
                        return true;
                }
            }

            return false;
        }

        private static IReadOnlyCollection<string> ExtractABAs(string val)
        {
            var abas = new List<string>();
            foreach (string s in val.Split('[', ']').Where((s, i) => i % 2 == 0))
            {
                for (int i = 0; i <= s.Length - 3; i++)
                {
                    string str = s.Substring(i, 3);
                    if (str[0] != str[1] && str[0] == str[2])
                        abas.Add(str);
                }
            }

            return abas;
        }
    }
}