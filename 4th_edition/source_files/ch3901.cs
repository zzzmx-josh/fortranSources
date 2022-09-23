using System;
using System.Net;
using System.Net.Sockets;
using System.IO;
using System.Text;

class ch3901
{

  static int Main()
  {

    const int n_sites=37;

    string base_address = 
    @"http://www.metoffice.gov.uk/pub/"
    +"data/weather/uk/climate/stationdata/";

    string [] station_name = 
    {
    "aberporth",       "armagh",      
    "ballypatrick",    "bradford",
    "braemar",         "camborne",    
    "cambridge",       "cardiff",
    "chivenor",        "cwmystwyth",  
    "dunstaffnage",    "durham",
    "eastbourne",      "eskdalemuir", 
    "heathrow",        "hurn",
    "lerwick",         "leuchars",    
    "lowestoft",       "manston",
    "nairn",           "newtonrigg",  
    "oxford",          "paisley",
    "ringway",         "rossonwye",   
    "shawbury",        "sheffield",
    "southampton",     "stornoway",   
    "suttonbonington", "tiree",
    "valley",          "waddington",  
    "whitby",          "wickairport",
    "yeovilton",
    };

    string [] web_address = new string[n_sites];

    string last_part="data.txt";

    string input_string;

    int i;

    // create the web address of each file

    for (i=0;i<n_sites;i++)
    {
      web_address[i]=
      base_address+station_name[i]+last_part;
      System.Console.WriteLine(web_address[i]);
    }

    string[] local_data_file = 
    {
      "aberporthdata.txt",        "armaghdata.txt",      
      "ballypatrickdata.txt",     "bradforddata.txt",
      "braemardata.txt",          "cambornedata.txt",    
      "cambridgedata.txt",        "cardiffdata.txt",
      "chivenordata.txt",         "cwmystwythdata.txt",  
      "dunstaffnagedata.txt",     "durhamdata.txt",
      "eastbournedata.txt",       "eskdalemuirdata.txt", 
      "heathrowdata.txt",         "hurndata.txt",
      "lerwickdata.txt",          "leucharsdata.txt",    
      "lowestoftdata.txt",        "manstondata.txt",
      "nairndata.txt",            "newtonriggdata.txt",  
      "oxforddata.txt",           "paisleydata.txt",
      "ringwaydata.txt",          "rossonwyedata.txt",   
      "shawburydata.txt",         "sheffielddata.txt",
      "southamptondata.txt",      "stornowaydata.txt",   
      "suttonboningtondata.txt",  "tireedata.txt",
      "valleydata.txt",           "waddingtondata.txt",  
      "whitbydata.txt",           "wickairportdata.txt",
      "yeoviltondata.txt"
    }; 


    StreamWriter output_file;

    for (i=0;i<n_sites;i++)
    {
      // create the web addresses

      HttpWebRequest  httpwreq   = (HttpWebRequest)
      WebRequest.Create(web_address[i]);

      // set up connection

      HttpWebResponse httpwresp  = (HttpWebResponse)
      httpwreq.GetResponse();

      // set up input stream

      StreamReader input_stream = new 
        StreamReader
        (httpwresp.GetResponseStream(),Encoding.ASCII);

      // read the whole file

      input_string=input_stream.ReadToEnd();

      // create the output file

      output_file = 
      File.CreateText("before_"+local_data_file[i]);

      output_file.WriteLine(input_string);

      input_stream.Close();
      output_file.Close();
    }
    return(0);
  }
}

