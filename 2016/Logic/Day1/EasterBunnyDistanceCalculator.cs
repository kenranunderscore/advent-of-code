namespace Logic.Day1
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Numerics;

    public class EasterBunnyDistanceCalculator
    {
        private static readonly Matrix3x2 right = new Matrix3x2(0, -1, 1, 0, 0, 0);
        private static readonly Matrix3x2 left = new Matrix3x2(0, 1, -1, 0, 0, 0);

        public int Distance(string input)
        {
            var currentDirection = new Vector2(0, 1);
            var currentPosition = new Vector2(0, 0);

            var commands =
                input
                    .Split(new[] { ", " }, StringSplitOptions.RemoveEmptyEntries)
                    .Select(x => (x[0] == 'R' ? right : left, int.Parse(x.Substring(1))));
            foreach (var command in commands)
            {
                currentDirection = AdvanceDirection(currentDirection, command);
                currentPosition += currentDirection * command.Item2;
            }

            return DistanceFromOrigin(currentPosition);
        }

        public int DistanceToFirstDoublyVisitedLocation(string input)
        {
            var currentDirection = new Vector2(0, 1);
            var currentPosition = new Vector2(0, 0);
            var visitedLocations = new HashSet<Vector2> { currentPosition };

            var commands =
                input
                    .Split(new[] { ", " }, StringSplitOptions.RemoveEmptyEntries)
                    .Select(x => (x[0] == 'R' ? right : left, int.Parse(x.Substring(1))));
            foreach (var command in commands)
            {
                currentDirection = AdvanceDirection(currentDirection, command);
                for (int i = 0; i < command.Item2; ++i)
                {
                    currentPosition += currentDirection;
                    if (visitedLocations.Contains(currentPosition))
                    {
                        return DistanceFromOrigin(currentPosition);
                    }

                    visitedLocations.Add(currentPosition);
                }
            }

            return DistanceFromOrigin(currentPosition);
        }

        private static Vector2 AdvanceDirection(Vector2 direction, (Matrix3x2, int) command) => FakeMultiply(command.Item1, direction);

        private static int DistanceFromOrigin(Vector2 position) => (int)(Math.Abs(position.X) + Math.Abs(position.Y));

        private static Vector2 FakeMultiply(Matrix3x2 m, Vector2 v) =>
            new Vector2(m.M11 * v.X + m.M12 * v.Y, m.M21 * v.X + m.M22 * v.Y);
    }
}