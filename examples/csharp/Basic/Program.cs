using LibLogit;

var logIT = new Logit
{
    LocalPath = "logs/csharp-app.log",
    Level = Level.Debug
};

logIT.At(Level.Info).Append("C# app started").Commit();

var configured = Logit.LoadLogits("examples/config/v2-basic.json");
configured["AppLog"].At(Level.Info).Append("configured C# log").Commit();
