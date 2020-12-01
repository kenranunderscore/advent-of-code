namespace Logic.Day5
{
    using System.Linq;
    using System.Security.Cryptography;
    using System.Text;

    public class PasswordDecoder
    {
        public string Decode(string input)
        {
            string password = string.Empty;
            int i = 0;
            while (password.Length < 8)
            {
                string hash = HexMD5Hash($"{input}{i}");
                if (hash.StartsWith("00000"))
                {
                    password += hash[5];
                }
                ++i;
            }

            return password;
        }

        public string DecodePart2(string input)
        {
            char[] password = new char[8];
            int i = 0;
            while (!IsFull(password))
            {
                string hash = HexMD5Hash($"{input}{i}");
                if (hash.StartsWith("00000"))
                {
                    char c = hash[5];
                    if (char.IsDigit(c))
                    {
                        int n = int.Parse(c.ToString());
                        if (n <= 7 && password[n] == '\0')
                        {
                            password[n] = hash[6];
                        }
                    }
                }
                ++i;
            }

            return new string(password);
        }

        private static bool IsFull(char[] password) => !password.Any(c => c == '\0');

        private static string HexMD5Hash(string value)
        {
            using (var md5 = MD5.Create())
            {
                var buf = Encoding.UTF8.GetBytes(value);
                var hash = md5.ComputeHash(buf);

                var builder = new StringBuilder();
                for (int i = 0; i < hash.Length; ++i)
                {
                    builder.Append(hash[i].ToString("x2"));
                }

                return builder.ToString();
            }
        }
    }
}