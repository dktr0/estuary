"use strict";

export function onLaunch(insertSound) { // in reality this would be more like onLaunch(api)
  console.log("proof of concept of using a JS module to add an audio resource");
  insertSound("samples/tink/000_tink1.wav","apitest",0); // and this would then be api.insertSound(...)
  }

