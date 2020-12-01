namespace Tests
{
    using Logic.Day2;
    using NUnit.Framework;
    using Tests.Inputs;

    [TestFixture]
    public class Day2_Test
    {
        [Test]
        public void Day2_Part1()
        {
            var input = Utils.ReadLines("day2_data.txt");
            var day2 = new BathroomCodeDetector();
            Assert.That(day2.DecipherCode(input, BathroomCodeDetector.NumpadMapping), Is.EqualTo("92435"));
        }

        [Test]
        public void Day2_Part2()
        {
            var input = Utils.ReadLines("day2_data.txt");
            var day2 = new BathroomCodeDetector();
            Assert.That(day2.DecipherCode(input, BathroomCodeDetector.Part2Mapping), Is.EqualTo("C1A88"));
        }
    }
}