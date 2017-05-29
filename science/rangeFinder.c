#include <fcntl.h>
#include <termios.h>
#include <unistd.h>
#include <stdio.h>
#include "leptsci.h"
#include "stdlib.h"
#include "string.h"
#include "MQTTClient.h"

#define ADDRESS     "tcp://localhost:1883"
#define CLIENTID    "ExampleClientPub"
#define TOPIC       "MQTT Examples"
#define PAYLOAD     "Hello World!"
#define QOS         1
#define TIMEOUT     10000L

void readLidar();
void readImage();

static unsigned short img[60][80];
static char hhead[] = "HTTP/1.1 200 OK\r\nContent-Type: image/x-bmp\r\n\r\n";
static unsigned char bmphead[] = {
    0x42, 0x4D, 0xF6, 0x16, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x36, 0x04, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0x50, 0x00, 0x00,
    0x00, 0x3C, 0x00, 0x00, 0x00, 0x01, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

int main(){
  readImage();
  // readLidar();
  // int isOpen = leptopen();
  // printf("Is open: %d\n", isOpen);
  return 0;
}


static void writebmp(void)
{

    int x, y;
    unsigned short minval = 0xffff, maxval = 0;
    for (y = 0; y < 60; y++)
        for (x = 0; x < 80; x++) {
            if (img[y][x] > maxval)
                maxval = img[y][x];
            if (img[y][x] < minval)
                minval = img[y][x];
        }
    maxval -= minval;           // span
    //    printf("%d + %d\n", minval, maxval);
printf("maxval: %d minval: %d", maxval, minval);

    write(1, hhead, sizeof(hhead) - 1); // no trailing null
    write(1, bmphead, sizeof(bmphead));
    int b, r, g, a = 0, val;
    for (val = 0; val < 256; val++) {
        b = r = g = val;
        write(1, &b, 1);
        write(1, &g, 1);
        write(1, &r, 1);
        write(1, &a, 1);
    }
printf("6------------------------\n");
printf("maxval: %d minval: %d", maxval, minval);
    for (y = 59; y >= 0; y--)
        for (x = 0; x < 80; x++) {
            val = ((img[y][x] - minval) * 255) / (maxval);
            val &= 0xff;
            write(1, &val, 1);
        }
}

void readImage(){
  // while(1){
    if(leptopen()){
      // while(leptget((unsigned short *) img)){
      int r = leptget((unsigned short *) img);
      printf("%d\n", r);
      if(0){
        // writebmp();
        usleep(100000);
        // leptclose();
      }

        // break;
      // }
    }
  // }
}


void readLidar(){
  int fd; /* port file descriptor */
  char portT[20] = "/dev/ttyUSB"; /* port to connect to */
  char port[20] = "/dev/ttyUSB"; /* port to connect to */
  speed_t baud = B115200; /* baud rate */
  while(1){
    for(int i = 0; i < 20; i++){
      sprintf(port, "%s%d", portT, i);
      fd = open(port, O_RDWR); /* connect to port */
      if(fd != -1){
        printf("Opened on: %s\n", port);
        break;
      }
    }
    /* set the other settings (in this case, 9600 8N1) */
    struct termios settings;
    tcgetattr(fd, &settings);

    cfsetospeed(&settings, baud); /* baud rate */
    settings.c_cflag &= ~PARENB; /* no parity */
    settings.c_cflag &= ~CSTOPB; /* 1 stop bit */
    settings.c_cflag &= ~CSIZE;
    settings.c_cflag |= CS8 | CLOCAL; /* 8 bits */
    settings.c_lflag = ICANON; /* canonical mode */
    settings.c_oflag &= ~OPOST; /* raw output */

    tcsetattr(fd, TCSANOW, &settings); /* apply the settings */
    tcflush(fd, TCOFLUSH);

    char buf[1024] = "";
    double distance = -1.0;
    float vReading = -1.0;
    float perc = -1.0;
    size_t readBytes = read(fd, buf, 1024);
    /* — code to use the port here — */
    while((int)readBytes > 0){
      readBytes = read(fd, buf, 1024);
      // printf("%s",buf);
      int res = sscanf(buf, "%lf m %f V %f %% \r\n", &distance, &vReading, &perc);
      if(res){
        printf("%lf\n",distance);
      }
      usleep(5000);
    }

    printf("Restarting range finder\n");
    sleep(1);
  }


  close(fd); /* cleanup */
}
