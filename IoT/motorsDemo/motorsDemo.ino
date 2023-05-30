
#define BUZZER_PIN 6
#define LED_PIN 13

#define BUTTON_PIN 12

#define MOTOR_A1 25
#define MOTOR_A2 26
#define MOTOR_B1 27
#define MOTOR_B2 14

void declarePins(){
  pinMode(MOTOR_A1, OUTPUT);
  pinMode(MOTOR_A2, OUTPUT);
  pinMode(MOTOR_B1, OUTPUT);
  pinMode(MOTOR_B2, OUTPUT);
}

void move_forward(int speed){
  analogWrite(MOTOR_A1, speed);
  analogWrite(MOTOR_A2, 0);
  analogWrite(MOTOR_B1, speed);
  analogWrite(MOTOR_B2, 0);
}
void move_backward(int speed){
  analogWrite(MOTOR_A1, 0);
  analogWrite(MOTOR_A2, speed);
  analogWrite(MOTOR_B1, 0);
  analogWrite(MOTOR_B2, speed);
}
void move_left(int speed){
  analogWrite(MOTOR_A1, 0);
  analogWrite(MOTOR_A2, speed);
  analogWrite(MOTOR_B1, speed);
  analogWrite(MOTOR_B2, 0);
}
void move_right(int speed){
  analogWrite(MOTOR_A1, speed);
  analogWrite(MOTOR_A2, 0);
  analogWrite(MOTOR_B1, 0);
  analogWrite(MOTOR_B2, speed);
}
void motor_stop(){
  analogWrite(MOTOR_A1, 0);
  analogWrite(MOTOR_A2, 0);
  analogWrite(MOTOR_B1, 0);
  analogWrite(MOTOR_B2, 0);
}
void move(char direction, int speed){
  switch(direction){
    case 'f':
      move_forward(speed);
      break;
    case 'b':
      move_backward(speed);
      break;
    case 'l':
      move_left(speed);
      break;
    case 'r':
      move_right(speed);
      break;
    case 's':
      motor_stop();
      break;
    default: 
      motor_stop();
  }
}
void motorTest(){
  move('f',120);
  delay(1000);
  move('b',120);
  delay(1000);
  move('l',120);
  delay(1000);
  move('r',120);
  delay(1000);
  move('s',120);
}
void setup() {
  declarePins();
}

void loop() {
  motorTest();
}
