unit proxyInfo;

interface

type
  TProxyAccessType = (patDirectConnection, patProxyConnection, patPreconfig);
  TProxyProtocol = (ppHTTP, ppSecure, ppFTP, ppGopher, ppSocks);

  TProxyInfo = record
    host: string;
    port: Integer;
    username: string;
    password: string;
  end;
  TProxyServers = array[TProxyProtocol] of TProxyInfo;

const
  proxyProtocolNames: array[TProxyProtocol] of string = ('http', 'https', 'ftp', 'gopher', 'socks');

// returns true if it retrieved the proxy settings, false if something went wrong
//function GetDefaultProxyServer(var proxyServer, bypassList: string; var port: Integer; var accessType: TProxyAccessType): Boolean;

function GetDefaultProxyServer(var proxyServers: TProxyServers; var bypassList: string; var accessType: TProxyAccessType): Boolean;

implementation

uses
  misc,
  Windows, wininet, sysUtils, classes;

//function GetDefaultProxyServer(var proxyServer, bypassList: string; var port: Integer; var accessType: TProxyAccessType): Boolean;
function GetDefaultProxyServer(var proxyServers: TProxyServers; var bypassList: string; var accessType: TProxyAccessType): Boolean;
// returns true if it retrieved the proxy settings

  function ParseProxyString(proxyString: string): TProxyServers;
  // Extract the server and port numbers.
  // The proxy string is of the form
  //   http=httpproxy:port ftp=ftpproxy:port
  // or
  //   generalproxy:port
  var
    portString, protocolName, protocolValue: string;
    protocol: TProxyProtocol;
    individualProxy, proxyList: string;
    proxyStringList: TStringList;
    i: Integer;
    spaceFound, universalProxyFound: Boolean;
    host: string;
    port: Integer;
    buffer: pointer;
    bufferLength: DWORD;
  begin
    // set the proxy servers to say no proxys (unless they are otherwise
    // written to)
    for protocol := Low(TProxyProtocol) to High(TProxyProtocol) do
    begin
      Result[protocol].host := '';
      Result[protocol].port := 0;
    end;

    if (proxyString <> '') then
    begin
      // split up the proxys into a TStringlist, use space as the delimiter
      proxyStringList := TStringList.Create;
      proxyList := proxyString;
      repeat
        spaceFound := splitString(' ', proxyList, individualProxy, proxyList);
        proxyStringList.Add(individualProxy);
      until not spaceFound;

      // discard any lines without ports as malformed, this shouldn't happen
      // but allows the rest of the code to assume the presence of a colon.
      i := 0;
      while i < proxyStringList.Count do
      begin
        if Pos(':', proxyStringList[i]) = 0 then
        begin
          // assume this line is malformed - no port
          proxyStringList.Delete(i);
        end
        else
        begin
          // the line is ok, check the next one
          Inc(i);
        end;
      end;

      // assign the first proxy found without an = to all protocols, and then
      // remove all lines without an = from the proxyStringList, allowing the
      // rest of the code to assume each remaining line is a key/value pair
      // (this takes care of basic proxy config strings, also malformed
      // advanced proxy config strings)
      universalProxyFound := false;
      i := 0;
      while i < proxyStringList.Count do
      begin
        if Pos('=', proxyStringList[i]) = 0 then
        begin
          // this line is not of the form <protocol>=<proxyhost>:<proxyport>
          // (it has no protocol specification)
          // so we've found a universal proxy

          if not universalProxyFound then
          begin
            // we've found the first universal proxy, assign it to all protocols
            // if it's well formed
            splitString(':', proxyStringList[i], host, portString);
            port := StrToIntDef(portString, 0);

            if (port > 0) and (port <= 65535) then
            begin
              // the proxy seems well formed
              universalProxyFound := true;

              for protocol := Low(TProxyProtocol) to High(TProxyProtocol) do
              begin
                Result[protocol].host := host;
                Result[protocol].port := port;

                GetMem(buffer, bufferLength);
                if InternetQueryOption(nil, INTERNET_OPTION_PROXY_PASSWORD, buffer, bufferLength) then
                begin
                  Result[Protocol].password := Copy(String(buffer),1,Length(String(buffer)));
                end;

                Dispose(buffer);
                GetMem(buffer, bufferLength);
                if InternetQueryOption(nil, INTERNET_OPTION_PROXY_USERNAME, buffer, bufferLength) then
                begin
                  Result[Protocol].username := Copy(String(buffer),1,Length(String(buffer)));
                end;
                Dispose(buffer);

              end;
            end;
          end;

          // remove it from the list and don't increment i - when this loop is
          // finished we will be able to assume all entries still in the list
          // are protocol specific key/value pairs
          proxyStringList.Delete(i);
        end
        else
        begin
          Inc(i);
        end;
      end;

      // all remaining lines should be of the form
      //   <protocol>=<proxyhost>:<proxyport>
      //
      // assign any specified proxys to their specified protocol
      for protocol := Low(TProxyProtocol) to High(TProxyProtocol) do
      begin
        protocolName := proxyProtocolNames[protocol];
        protocolValue := proxyStringList.Values[protocolName]; // note that this lookup is not case sensitive (yay!)

        splitString(':', protocolValue, host, portString);
        port := StrToIntDef(portString, 0);

        if (port > 0) and (port <= 65535) then
        begin
          // the proxy seems well formed
          Result[protocol].host := host;
          Result[protocol].port := port;

          GetMem(buffer, bufferLength);
          if InternetQueryOption(nil, INTERNET_OPTION_PROXY_PASSWORD, buffer, bufferLength) then
          begin
            Result[Protocol].password := Copy(String(buffer),1,Length(String(buffer)));
          end;

          Dispose(buffer);
          GetMem(buffer, bufferLength);
          if InternetQueryOption(nil, INTERNET_OPTION_PROXY_USERNAME, buffer, bufferLength) then
          begin
            Result[Protocol].username := Copy(String(buffer),1,Length(String(buffer)));
          end;
          Dispose(buffer);
      
        end;
      end;

      FreeAndNil(proxyStringList);
    end;
  end;


var
  ProxyInfo: PInternetProxyInfo;
  buffer: pointer;
  bufferLength: DWORD;
begin
  Result := false;

  // obtain the length of the buffer required
  bufferLength := 0;
  InternetQueryOption(nil, INTERNET_OPTION_PROXY, nil, bufferLength);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    // bufferLength should now have been set
    GetMem(buffer, bufferLength);
    if InternetQueryOption(nil, INTERNET_OPTION_PROXY, buffer, bufferLength) then
    begin
      ProxyInfo := buffer;
      bypassList := ProxyInfo^.lpszProxyBypass;

      proxyServers := ParseProxyString(ProxyInfo^.lpszProxy);

      case ProxyInfo^.dwAccessType of
        INTERNET_OPEN_TYPE_DIRECT:    accessType := patDirectConnection;
        INTERNET_OPEN_TYPE_PRECONFIG: accessType := patPreconfig;
        INTERNET_OPEN_TYPE_PROXY:     accessType := patProxyConnection;
      end;

      Result := true;
    end;

    FreeMem(buffer);
  end;
end;

end.
