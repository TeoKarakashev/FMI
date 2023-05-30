#define MOTOR_RIGHT_FRONT1 13
#define MOTOR_RIGHT_FRONT2 12
#define MOTOR_RIGHT_BACK1 14
#define MOTOR_RIGHT_BACK2 27

#define MOTOR_LEFT_FRONT1 25
#define MOTOR_LEFT_FRONT2 26
#define MOTOR_LEFT_BACK1 32
#define MOTOR_LEFT_BACK2 33


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
void move(char direction, int speed) {
  switch (direction) {
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
void motorTest() {
  move('f', 120);
  delay(1000);
  move('b', 120);
  delay(1000);
  move('l', 120);
  delay(1000);
  move('r', 120);
  delay(1000);
  move('s', 120);
  delay(1000);
}
void setup() {
  declarePins();
}

void loop() {
  //move('l', 120);
  //delay(1000);
  motorTest();
}
