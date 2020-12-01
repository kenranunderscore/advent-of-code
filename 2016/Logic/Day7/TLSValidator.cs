namespace Logic.Day7
{
    using System.Linq;
    using System.Text.RegularExpressions;

    public class TLSValidator
    {
        private static readonly Regex bracketContentRegex = new Regex(@"\[(\w+)\]", RegexOptions.IgnoreCase);

        public bool SupportsTLS(string sequence)
        {
            foreach (var match in bracketContentRegex.Matches(sequence).Cast<Match>())
            {
                if (HasABBA(match.Groups[1].Value))
                    return false;
            }

            return sequence.Split('[', ']')
                .Where((s, i) => i % 2 == 0)
                .Any(HasABBA);
        }

        private static bool HasABBA(string val)
        {
            for (int i = 0; i <= val.Length - 4; i++)
            {
                string str = val.Substring(i, 4);
                if (str[0] != str[1] && str[0] == str[3] && str[1] == str[2])
                    return true;
            }

            return false;
        }
    }
}