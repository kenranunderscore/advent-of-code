namespace Logic.Test;

using Logic.Day12;
using NUnit.Framework;
using System.Linq;

[TestFixture]
public class Day12_Test
{
    [Test]
    public void Day12_Part1()
    {
        var input = Utils.ReadLines("day12_data.txt").ToList();
        var interpreter = new AssembunnyInterpreter(input);
        interpreter.Run();
        Assert.That(interpreter.Registers["a"], Is.EqualTo(318009));
    }

[Test]
public void Day12_Part2()
    {
        var input = Utils.ReadLines("day12_data.txt").ToList();
        var interpreter = new AssembunnyInterpreter(input);
        interpreter.Registers["c"] = 1;
        interpreter.Run();
        Assert.That(interpreter.Registers["a"], Is.EqualTo(9227663));
    }
}
