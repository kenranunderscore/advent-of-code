namespace Tests
{
    using Logic.Day10;
    using NUnit.Framework;
    using Tests.Inputs;

    [TestFixture]
    public class Day10_Test
    {
        [Test]
        public void Day10_Part1()
        {
            var orders = Utils.ReadLines("day10_data.txt");
            var botController = new BotController();
            Assert.That(botController.FindBotHandlingNumbers(orders, 17, 61), Is.EqualTo(113));
        }

        [Test]
        public void Day10_Part2()
        {
            var orders = Utils.ReadLines("day10_data.txt");
            var botController = new BotController();
            botController.FindBotHandlingNumbers(orders, 17, 61);
            Assert.That(botController.Part2, Is.EqualTo(12803));
        }
    }
}