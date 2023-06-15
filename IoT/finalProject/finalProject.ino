#include <AsyncTCP.h>
#include <ESPAsyncWebServer.h>
#include <WiFi.h>
#include <AsyncTCP.h>
#include <WiFiClient.h>
#include <WiFiAP.h>

#define MOTOR_RIGHT_FRONT1 13
#define MOTOR_RIGHT_FRONT2 12
#define MOTOR_RIGHT_BACK1 14
#define MOTOR_RIGHT_BACK2 27

#define MOTOR_LEFT_FRONT1 25
#define MOTOR_LEFT_FRONT2 26
#define MOTOR_LEFT_BACK1 32
#define MOTOR_LEFT_BACK2 33

#define UP 1
#define DOWN 2
#define LEFT 3
#define RIGHT 4
#define UP_LEFT 5
#define UP_RIGHT 6
#define DOWN_LEFT 7
#define DOWN_RIGHT 8
#define STOP 0

#define SPEED 120


void declarePins() {
  pinMode(MOTOR_RIGHT_FRONT1, OUTPUT);
  pinMode(MOTOR_RIGHT_FRONT2, OUTPUT);
  pinMode(MOTOR_RIGHT_BACK1, OUTPUT);
  pinMode(MOTOR_RIGHT_BACK2, OUTPUT);
  pinMode(MOTOR_LEFT_FRONT1, OUTPUT);
  pinMode(MOTOR_LEFT_FRONT2, OUTPUT);
  pinMode(MOTOR_LEFT_BACK1, OUTPUT);
  pinMode(MOTOR_LEFT_BACK2, OUTPUT);
}

const char* ssid     = "wifiCar";
const char* password = "12345678910";

AsyncWebServer server(80);
AsyncWebSocket ws("/ws");

const char* htmlHomePage PROGMEM = R"HTMLHOMEPAGE(
<!DOCTYPE html>
<html>
  <head>
  <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no">
    <style>
    .arrows {
      font-size:70px;
      color:red;
    }
    .circularArrows {
      font-size:80px;
      color:blue;
    }
    td {
      background-color:black;
      border-radius:25%;
      box-shadow: 5px 5px #888888;
    }
    td:active {
      transform: translate(5px,5px);
      box-shadow: none; 
    }

    .noselect {
      -webkit-touch-callout: none; /* iOS Safari */
        -webkit-user-select: none; /* Safari */
         -khtml-user-select: none; /* Konqueror HTML */
           -moz-user-select: none; /* Firefox */
            -ms-user-select: none; /* Internet Explorer/Edge */
                user-select: none; /* Non-prefixed version, currently
                                      supported by Chrome and Opera */
    }
    </style>
  </head>
  <body class="noselect" align="center" style="background-color:white">
    
    <table id="mainTable" style="width:400px;margin:auto;table-layout:fixed" CELLSPACING=10>
      <tr>
        <td ontouchstart='onTouchStartAndEnd("5")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#11017;</span></td>
        <td ontouchstart='onTouchStartAndEnd("1")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#8679;</span></td>
        <td ontouchstart='onTouchStartAndEnd("6")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#11016;</span></td>
      </tr>
      
      <tr>
        <td ontouchstart='onTouchStartAndEnd("3")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#8678;</span></td>
        <td></td>    
        <td ontouchstart='onTouchStartAndEnd("4")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#8680;</span></td>
      </tr>
      
      <tr>
        <td ontouchstart='onTouchStartAndEnd("7")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#11019;</span></td>
        <td ontouchstart='onTouchStartAndEnd("2")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#8681;</span></td>
        <td ontouchstart='onTouchStartAndEnd("8")' ontouchend='onTouchStartAndEnd("0")'><span class="arrows" >&#11018;</span></td>
      </tr>
    </table>

    <script>
      var webSocketUrl = "ws:\/\/" + window.location.hostname + "/ws";
      var websocket;
      
      function initWebSocket() 
      {
        websocket = new WebSocket(webSocketUrl);
        websocket.onopen    = function(event){};
        websocket.onclose   = function(event){setTimeout(initWebSocket, 2000);};
        websocket.onmessage = function(event){};
      }

      function onTouchStartAndEnd(value) 
      {
        websocket.send(value);
      }
          
      window.onload = initWebSocket;
      document.getElementById("mainTable").addEventListener("touchend", function(event){
        event.preventDefault()
      });      
    </script>
    
  </body>
</html> 

)HTMLHOMEPAGE";

void processCarMovement(String inputValue) {
 // Serial.println("Got value as %s %d\n", inputValue.c_str(), inputValue.toInt());  
  switch(inputValue.toInt()) {
    case UP:
      move_forward(SPEED);                
      break;
      
    case DOWN:
      move_backward(SPEED);    
      break;
      
    case LEFT:
      move_left(SPEED);
      break;
  
    case RIGHT:
      move_right(SPEED);
      break;
  
    case UP_LEFT:
      move_forward_left(SPEED);
      break;
  
    case UP_RIGHT:
      move_forward_right(SPEED); 
      break;
  
    case DOWN_LEFT:
      move_backward_left(SPEED); 
      break;
  
    case DOWN_RIGHT:
      move_backward_right(SPEED);  
      break;
  
    case STOP:
      motor_stop(); 
      break;
  
    default:
      motor_stop();    
      break;
  }
}

void move_forward(int speed) {
  analogWrite(MOTOR_RIGHT_FRONT1, speed);
  analogWrite(MOTOR_RIGHT_FRONT2, 0);
  analogWrite(MOTOR_RIGHT_BACK1, speed);
  analogWrite(MOTOR_RIGHT_BACK2, 0);
  analogWrite(MOTOR_LEFT_FRONT1, speed);
  analogWrite(MOTOR_LEFT_FRONT2, 0);
  analogWrite(MOTOR_LEFT_BACK1, speed);
  analogWrite(MOTOR_LEFT_BACK2, 0);
}

void move_forward_left(int speed) {
  int halfSpeed = speed / 2;
  analogWrite(MOTOR_RIGHT_FRONT1, speed);
  analogWrite(MOTOR_RIGHT_FRONT2, 0);
  analogWrite(MOTOR_RIGHT_BACK1, speed);
  analogWrite(MOTOR_RIGHT_BACK2, 0);
  analogWrite(MOTOR_LEFT_FRONT1, halfSpeed);
  analogWrite(MOTOR_LEFT_FRONT2, 0);
  analogWrite(MOTOR_LEFT_BACK1, halfSpeed);
  analogWrite(MOTOR_LEFT_BACK2, 0);
}

void move_forward_right(int speed) {
  int halfSpeed = speed / 2;
  analogWrite(MOTOR_RIGHT_FRONT1, halfSpeed);
  analogWrite(MOTOR_RIGHT_FRONT2, 0);
  analogWrite(MOTOR_RIGHT_BACK1, halfSpeed);
  analogWrite(MOTOR_RIGHT_BACK2, 0);
  analogWrite(MOTOR_LEFT_FRONT1, speed);
  analogWrite(MOTOR_LEFT_FRONT2, 0);
  analogWrite(MOTOR_LEFT_BACK1, speed);
  analogWrite(MOTOR_LEFT_BACK2, 0);
}

void move_backward(int speed) {
  analogWrite(MOTOR_RIGHT_FRONT1, 0);
  analogWrite(MOTOR_RIGHT_FRONT2, speed);
  analogWrite(MOTOR_RIGHT_BACK1, 0);
  analogWrite(MOTOR_RIGHT_BACK2, speed);
  analogWrite(MOTOR_LEFT_FRONT1, 0);
  analogWrite(MOTOR_LEFT_FRONT2, speed);
  analogWrite(MOTOR_LEFT_BACK1, 0);
  analogWrite(MOTOR_LEFT_BACK2, speed);
}

void move_backward_left(int speed) {
  int halfSpeed = speed / 2;
  analogWrite(MOTOR_RIGHT_FRONT1, 0);
  analogWrite(MOTOR_RIGHT_FRONT2, speed);
  analogWrite(MOTOR_RIGHT_BACK1, 0);
  analogWrite(MOTOR_RIGHT_BACK2, speed);
  analogWrite(MOTOR_LEFT_FRONT1, 0);
  analogWrite(MOTOR_LEFT_FRONT2, halfSpeed);
  analogWrite(MOTOR_LEFT_BACK1, 0);
  analogWrite(MOTOR_LEFT_BACK2, halfSpeed);
}

void move_backward_right(int speed) {
  int halfSpeed = speed / 2;
  analogWrite(MOTOR_RIGHT_FRONT1, 0);
  analogWrite(MOTOR_RIGHT_FRONT2, halfSpeed);
  analogWrite(MOTOR_RIGHT_BACK1, 0);
  analogWrite(MOTOR_RIGHT_BACK2, halfSpeed);
  analogWrite(MOTOR_LEFT_FRONT1, 0);
  analogWrite(MOTOR_LEFT_FRONT2, speed);
  analogWrite(MOTOR_LEFT_BACK1, 0);
  analogWrite(MOTOR_LEFT_BACK2, speed);
}

void move_left(int speed) {
  analogWrite(MOTOR_RIGHT_FRONT1, speed);
  analogWrite(MOTOR_RIGHT_FRONT2, 0);
  analogWrite(MOTOR_RIGHT_BACK1, speed);
  analogWrite(MOTOR_RIGHT_BACK2, 0);
  analogWrite(MOTOR_LEFT_FRONT1, 0);
  analogWrite(MOTOR_LEFT_FRONT2, speed);
  analogWrite(MOTOR_LEFT_BACK1, 0);
  analogWrite(MOTOR_LEFT_BACK2, speed);
}
void move_right(int speed) {
  analogWrite(MOTOR_RIGHT_FRONT1, 0);
  analogWrite(MOTOR_RIGHT_FRONT2, speed);
  analogWrite(MOTOR_RIGHT_BACK1, 0);
  analogWrite(MOTOR_RIGHT_BACK2, speed);
  analogWrite(MOTOR_LEFT_FRONT1, speed);
  analogWrite(MOTOR_LEFT_FRONT2, 0);
  analogWrite(MOTOR_LEFT_BACK1, speed);
  analogWrite(MOTOR_LEFT_BACK2, 0);
}
void motor_stop() {
  analogWrite(MOTOR_RIGHT_FRONT1, 0);
  analogWrite(MOTOR_RIGHT_FRONT2, 0);
  analogWrite(MOTOR_RIGHT_BACK1, 0);
  analogWrite(MOTOR_RIGHT_BACK2, 0);
  analogWrite(MOTOR_LEFT_FRONT1, 0);
  analogWrite(MOTOR_LEFT_FRONT2, 0);
  analogWrite(MOTOR_LEFT_BACK1, 0);
  analogWrite(MOTOR_LEFT_BACK2, 0);
}

void handleRoot(AsyncWebServerRequest *request) {
  request->send_P(200, "text/html", htmlHomePage);
}

void handleNotFound(AsyncWebServerRequest *request) {
    request->send(404, "text/plain", "File Not Found!");
}


void onWebSocketEvent(AsyncWebSocket *server, AsyncWebSocketClient *client, AwsEventType type,
                      void *arg, uint8_t *data,  size_t len) {                      
  switch (type) {
    case WS_EVT_CONNECT:
      //Serial.println("WebSocket client #%u connected from %s\n", client->id(), client->remoteIP().toString().c_str());
      //client->text(getRelayPinsStatusJson(ALL_RELAY_PINS_INDEX));
      break;
    case WS_EVT_DISCONNECT:
      //Serial.println("WebSocket client #%u disconnected\n", client->id());
      processCarMovement("0");
      break;
    case WS_EVT_DATA:
      AwsFrameInfo *info;
      info = (AwsFrameInfo*)arg;
      if (info->final && info->index == 0 && info->len == len && info->opcode == WS_TEXT) {
        std::string myData = "";
        myData.assign((char *)data, len);
        processCarMovement(myData.c_str());       
      }
      break;
    case WS_EVT_PONG:
    case WS_EVT_ERROR:
      break;
    default:
      break;  
  }
}

void setup() {
  declarePins();
  Serial.begin(115200);

  WiFi.softAP(ssid, password);
  IPAddress IP = WiFi.softAPIP();
  Serial.print("AP IP address: ");
  Serial.println(IP);

  server.on("/", HTTP_GET, handleRoot);
  server.onNotFound(handleNotFound);
  
  ws.onEvent(onWebSocketEvent);
  server.addHandler(&ws);
  
  server.begin();
  Serial.println("HTTP server started");
}

void motorTest(){
  delay(1000);
  move_forward(120);
   delay(1000);
  move_backward(120);
   delay(1000);
  move_left(120);
   delay(1000);
  move_right(120);
  delay(1000);
  motor_stop();
  delay(1000);
  
}


void loop() {
 // ws.cleanupClients(); 
  motorTest();
}
