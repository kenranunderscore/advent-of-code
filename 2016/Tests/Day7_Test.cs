namespace Tests
{
    using Logic.Day7;
    using NUnit.Framework;
    using System.Linq;
    using Tests.Inputs;

    [TestFixture]
    public class Day7_Test
    {
        [Test]
        public void Day7_Part1()
        {
            var input = Utils.ReadLines("day7_data.txt");
            var validator = new TLSValidator();
            int numberOfLinesThatSupportTLS = input.Count(validator.SupportsTLS);
            Assert.That(numberOfLinesThatSupportTLS, Is.EqualTo(115));
        }

        [Test]
        public void Day7_Part2()
        {
            var input = Utils.ReadLines("day7_data.txt");
            var validator = new SSLValidator();
            int numberOfLinesThatSupportTLS = input.Count(validator.SupportsSSL);
            Assert.That(numberOfLinesThatSupportTLS, Is.EqualTo(231));
        }

        [Test]
        public void Sample_String_That_Supports_TLS_Is_Judged_Correctly()
        {
            Assert.That(new TLSValidator().SupportsTLS("abba[mnop]qrst"));
        }

        [Test]
        public void Sample_String_That_Supports_SLS_Is_Judged_Correctly()
        {
            Assert.That(new SSLValidator().SupportsSSL("zazbz[bzb]cdb"));
        }

        [Test]
        public void Sample_String_That_Does_Not_Support_SLS_Is_Judged_Correctly()
        {
            Assert.That(new SSLValidator().SupportsSSL("xyx[xyx]xyx"), Is.False);
        }
    }
}