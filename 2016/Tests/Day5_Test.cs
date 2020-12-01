namespace Tests
{
    using Logic.Day5;
    using NUnit.Framework;

    [TestFixture]
    public class Day5_Test
    {
        [Test]
        public void Day5_Part1()
        {
            var decoder = new PasswordDecoder();
            Assert.That(decoder.Decode("uqwqemis"), Is.EqualTo("1a3099aa"));
        }

        [Test]
        public void Day5_Part2()
        {
            var decoder = new PasswordDecoder();
            Assert.That(decoder.DecodePart2("uqwqemis"), Is.EqualTo("694190cd"));
        }
    }
}