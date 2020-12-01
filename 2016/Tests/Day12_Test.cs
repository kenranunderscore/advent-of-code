namespace Tests
{
    using Logic.Day12;
    using NUnit.Framework;
    using System.Linq;
    using Tests.Inputs;

    [TestFixture]
    public class Day12_Test
    {
        [Test]
        public void Day12_Part1()
        {
            var interpreter = new AssembunnyInterpreter();
            interpreter.Input = Utils.ReadLines("day12_data.txt").ToList();
            interpreter.Run();
            Assert.That(interpreter.Registers["a"], Is.EqualTo(318009));
        }

        [Test]
        public void Day12_Part2()
        {
            var interpreter = new AssembunnyInterpreter();
            interpreter.Input = Utils.ReadLines("day12_data.txt").ToList();
            interpreter.Registers["c"] = 1;
            interpreter.Run();
            Assert.That(interpreter.Registers["a"], Is.EqualTo(9227663));
        }
    }
}