sudo apt-get update
sudo adduser soro audio
sudo adduser soro video
sudo adduser soro dialout
sudo apt-get install -y git curl build-essential python-dev python-pip qt5-default libsdl2-dev gstreamer1.0-* i965-va-driver libqt5gstreamer-dev mosquitto
sudo pip install adafruit-pca9685