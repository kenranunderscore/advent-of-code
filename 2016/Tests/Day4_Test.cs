namespace Tests
{
    using Logic.Day4;
    using NUnit.Framework;
    using Tests.Inputs;

    [TestFixture]
    public class Day4_Test
    {
        [Test]
        public void Day4_Part1()
        {
            var decrypter = new RoomNameDecrypter();
            var rooms = Utils.ReadLines("day4_data.txt");
            Assert.That(decrypter.SumOfSectorIdsOfRealRooms(rooms), Is.EqualTo(137896));
        }

        [Test]
        public void Day4_Part2()
        {
            var decrypter = new RoomNameDecrypter();
            var rooms = Utils.ReadLines("day4_data.txt");
            Assert.That(decrypter.NorthPoleStorageSectorId(rooms), Is.EqualTo(501));
        }

        [Test]
        public void Detects_Real_Rooms()
        {
            Assert.That(RoomNameDecrypter.IsRoomReal("aaaaa-bbb-z-y-x-123[abxyz]"), Is.True);
            Assert.That(RoomNameDecrypter.IsRoomReal("a-b-c-d-e-f-g-h-987[abcde]"), Is.True);
            Assert.That(RoomNameDecrypter.IsRoomReal("not-a-real-room-404[oarel]"), Is.True);
        }

        [Test]
        public void Rotate_Works_In_Simple_Case()
        {
            Assert.That(RoomNameDecrypter.Rotate('a', 10), Is.EqualTo('k'));
        }

        [Test]
        public void Rotate_Works_With_Overflow()
        {
            Assert.That(RoomNameDecrypter.Rotate('x', 5), Is.EqualTo('c'));
        }
    }
}