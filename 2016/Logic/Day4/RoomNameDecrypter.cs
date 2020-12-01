namespace Logic.Day4
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;

    public class RoomNameDecrypter
    {
        public int SumOfSectorIdsOfRealRooms(IEnumerable<string> rooms) =>
            rooms.Where(IsRoomReal).Sum(ExtractSectorIdFromRoom);

        public int NorthPoleStorageSectorId(IEnumerable<string> rooms) =>
            ExtractSectorIdFromRoom(
                rooms
                    .Where(IsRoomReal)
                    .Select(DecryptRealRoom)
                    .Single(r => r.Contains("northpole")));

        internal static string DecryptRealRoom(string room)
        {
            int sectorId = ExtractSectorIdFromRoom(room);
            return new string(room
                .Select(r => Rotate(r, sectorId))
                .ToArray());
        }

        internal static char Rotate(char c, int count)
        {
            int last = 'z';
            int first = 'a';
            if (c < first || c > last)
                return c;

            int diff = count % 26;
            int sum = (int)c + diff;
            if (sum > last)
                sum -= 26;

            return (char)sum;
        }

        internal static int ExtractSectorIdFromRoom(string room) => int.Parse(Regex.Match(room, @"\-(\d+)\[").Groups[1].Value);

        internal static bool IsRoomReal(string room)
        {
            string checksum = Regex.Match(room, @"\[(.*)\]").Groups[1].Value;
            string name = room.Substring(0, room.IndexOf('['));
            string strippedName = Regex.Replace(name, @"[-\d]", string.Empty);
            var correctChecksum = new string(strippedName
                .GroupBy(c => c, (c, g) => new { Char = c, Quantity = g.Count() })
                .OrderByDescending(x => x.Quantity)
                .ThenBy(x => x.Char)
                .Take(5)
                .Select(x => x.Char)
                .ToArray());
            return checksum == correctChecksum;
        }
    }
}