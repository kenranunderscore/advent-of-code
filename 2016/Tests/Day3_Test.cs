namespace Tests
{
    using Logic.Day3;
    using NUnit.Framework;
    using Tests.Inputs;

    [TestFixture]
    public class Day3_Test
    {
        [Test]
        public void Day3_Part1()
        {
            var input = Utils.ReadLines("day3_data.txt");
            var validator = new TriangleValidator();
            Assert.That(validator.NumberOfValidTriangles(input), Is.EqualTo(869));
        }

        [Test]
        public void Day3_Part2()
        {
            var input = Utils.ReadLines("day3_data.txt");
            var validator = new TriangleValidator();
            Assert.That(validator.NumberOfValidTrianglesInColumns(input), Is.EqualTo(1544));
        }
    }
}