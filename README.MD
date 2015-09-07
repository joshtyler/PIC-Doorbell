#PIC-Doorbell

A remotely controlled doorbell project created using two PIC16F84A microcontrollers, programmed in PIC Assembly.

This was a group project created for a university lab class. I was primarily responsible for the creation of the code, and schematic design, however I also assisted with PCB layout.

![Final image](https://raw.githubusercontent.com/joshtyler/PIC-Doorbell/master/docs/img/final.JPG "Remote and receiver together")

The project features a remote control which uses a custom IR modulation scheme to activate a tune on the receiver.

The receiver features the ability to play for pre-programmed tunes over six complete ocataves, as well as the ability to record a custom tune, which is stored in EEPROM memory.

Full assembly code, as well as a schematic and gerbers, is available for both the doorbell and receiver.

There is also a report in the docs folder which covers theory of operation as well as mechanical design.

## Notes

The project is split into two halves one for the transmitter and one for the receiver, each has a separate schematic and code.

Please note that whilst every effort has been made to ensure the files provided are correct, this is an old project and the files have been collated from an archive. Therefore no guarantees are made about these files!

It is especially recommended not to manufacture a PCB from the gerbers without first verifying the design!

## Known problems and limitations
The receiver code requires the tune tables not to jump over page boundaries. A long time was spent tackling this issue, but despite much investigation a correct scheme could not be made to work.

The report features one or two missing images.

##License
This project is released under the MIT license, see LICENSE.txt.

You may do whatever you please with this code, so long as attribution is given to me.