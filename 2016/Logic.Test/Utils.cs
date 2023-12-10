namespace Logic.Test;

using System.Collections.Generic;
using System.IO;
using System.Reflection;

public static class Utils
{
    private const string FolderPrefix = "Logic.Test.Inputs.";

    public static IEnumerable<string> ReadLines(string resourceName)
    {
        string fullName = FolderPrefix + resourceName;
        using (var stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(fullName))
        using (var reader = new StreamReader(stream))
        {
            string currentLine;
            while ((currentLine = reader.ReadLine()) != null)
            yield return currentLine;
        }
    }
}
