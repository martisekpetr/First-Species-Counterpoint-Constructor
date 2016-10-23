from midiutil.MidiFile3 import MIDIFile

lines = []
with open("out.txt") as f:
    lines = [line.strip("[]\n").split(',') for line in f]

MyMIDI = MIDIFile(2)
time = 0

MyMIDI.addTempo(0,time,120)
MyMIDI.addProgramChange(0, 0, time, 69)
MyMIDI.addProgramChange(1, 1, time, 69)
MyMIDI.addControllerEvent(0, 0, time, 10, 20)
MyMIDI.addControllerEvent(1, 1, time, 10, 104)


track = 0
channel = 0
volume = 100
duration = 2

for line in lines:
    for pitch in line:
        MyMIDI.addNote(track,channel,int(pitch.strip()),time,duration,volume)
        time += duration

    time = 0
    track += 1
    channel += 1

binfile = open("output.mid", 'wb')
MyMIDI.writeFile(binfile)
binfile.close()