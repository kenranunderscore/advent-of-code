namespace Tests
{
    using Logic.Day6;
    using NUnit.Framework;
    using Tests.Inputs;

    [TestFixture]
    public class Day6_Test
    {
        [Test]
        public void Day6_Part1()
        {
            var day6 = new RepetitionCodeDecipherer();
            Assert.That(day6.DeciperMessagePart1(Utils.ReadLines("day6_data.txt")), Is.EqualTo("ygjzvzib"));
        }

        [Test]
        public void Day6_Part2()
        {
            var day6 = new RepetitionCodeDecipherer();
            Assert.That(day6.DeciperMessagePart2(Utils.ReadLines("day6_data.txt")), Is.EqualTo("pdesmnoz"));
        }
    }
}