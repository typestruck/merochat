Gofer it
	smalltalkhubUser: 'DBXTalk' project: 'Garage';
	configurationOf: 'Garage';
	load.

(ConfigurationOfGarage project version: '0.1')
	load: 'postgresV2'.

Gofer it
    smalltalkhubUser: 'zeroflag' project: 'Teapot';
    configuration;
    loadStable.

Gofer it
   smalltalkhubUser: 'NorbertHartl' project: 'Mustache';
   configuration;
   loadStable.

Gofer new
    url:  'http://smalltalkhub.com/mc/UdoSchneider/PBKDF2/main';
    package: 'ConfigurationOfPBKDF2';
    load.

(Smalltalk at: #ConfigurationOfPBKDF2) project stableVersion load.       

Gofer new
   smalltalkhubUser: 'SvenVanCaekenberghe' project: 'ZincHTTPComponents';
   package: 'ConfigurationOfZincHTTPComponents';
   load.
(Smalltalk globals at: #ConfigurationOfZincHTTPComponents) project latestVersion load: 'WebSocket'.

Gofer new
url: 'http://smalltalkhub.com/mc/Pharo/Pharo60/main';
package:'Metacello-GitBasedRepository';
load.

Gofer new
url: 'http://smalltalkhub.com/mc/Pharo/Pharo60/main';
package:'Metacello-GitHub';
load.

ZnWebSocketDelegate>>teaEvalActionOnRequest: aTeaRequest 

        ^self handleRequest: aTeaRequest 

