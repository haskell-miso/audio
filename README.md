# 🍜 🔊 miso-audio

Sample web app with audio, in Haskell, using [Miso](https://haskell-miso.org/).

A Winamp-inspired audio player: playlist, transport controls, seek bar,
volume, and a little spectrum visualizer. Audio tracks are streamed from
[SoundHelix](https://www.soundhelix.com/audio-examples) (music by T. Schürger).


## Try online

- [https://audio.haskell-miso.org/](https://audio.haskell-miso.org/)


## Build and run

Install [Nix Flakes](https://nixos.wiki/wiki/Flakes), then:

```
nix develop
make
make serve
```


## References

- [HTML DOM Audio Object](https://www.w3schools.com/jsref/dom_obj_audio.asp)
- [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API)

