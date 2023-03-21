#include <arduino.h>
#include <NeoPixelBus.h>

const uint16_t PixelCount = 16; // this example assumes 4 pixels, making it smaller will cause a failure
const uint8_t PixelPin = 2;  // make sure to set this to the correct pin, ignored for Esp8266


#define Cred     RgbColor(255, 0, 0)
#define Cred2     RgbColor(164, 0, 0)
#define Cpink    RgbColor(255, 164, 0)
#define Clila    RgbColor(164, 164, 0)
#define Cviolet  RgbColor(164, 255, 0)
#define Cblue    RgbColor(0, 255, 0)
#define Cblue2    RgbColor(0, 164, 0)
#define Cmblue   RgbColor(0, 255, 200)
#define Cmblue2   RgbColor(0, 255, 164)
#define Ccyan    RgbColor(0, 164, 164)
#define Cgreen   RgbColor(0, 0, 255)
#define Cgreen   RgbColor(0, 0, 164)
#define Cyellow  RgbColor(164, 0, 255)
#define Corange  RgbColor(164, 0, 164)
#define Corange2  RgbColor(164, 164, 164)
#define Cwhite   RgbColor(255, 255, 255)
#define Cblack   RgbColor(0) 
RgbColor allColors[] = { Cred, Cred2, Cpink, Clila, Cviolet, Cblue, Cblue2, Cmblue, Cmblue2, Ccyan, Cgreen, Cgreen, Cyellow, Corange, Corange2, Cwhite };

// Uart method is good for the Esp-01 or other pin restricted modules
// NOTE: These will ignore the PIN and use GPI02 pin
//NeoPixelBus<NeoGrbFeature, NeoEsp8266Uart800KbpsMethod> strip(PixelCount, PixelPin);
NeoPixelBus<NeoGrbFeature, Neo800KbpsMethod> strip(PixelCount, PixelPin);
//NeoPixelBus<NeoRgbFeature, NeoEsp8266Uart400KbpsMethod> strip(PixelCount, PixelPin);


void setup() {
    Serial.begin(115200);
    while (!Serial); // wait for serial attach

    Serial.println();
    Serial.println("Initializing...");
    Serial.flush();

    // this resets all the neopixels to an off state
    strip.Begin();
    strip.Show();


    Serial.println();
    Serial.println("Running...");
}


void loop() {

    for (int i = 0; i < PixelCount; i++) {
        RgbColor c = allColors[i % 15];
        c = RgbColor::LinearBlend(c, Cblack, 0.9F);
        strip.SetPixelColor(i, c);
        strip.Show();
        delay(500);
    }
    delay(1000);

    Serial.println("Off ...");
    for (int i = 0; i < PixelCount; i++) {
        strip.SetPixelColor(i, Cblack);
        strip.Show();
        delay(100);
    }

    //    delay(1000);


}