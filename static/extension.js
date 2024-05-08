"use strict";

export function onLaunch(api) {
  console.log(api);
  console.log("proof of concept of using a JS module to manage ensemble audio resources");
  api.clearResources();
  // api.defaultResources();
  api.reslist("https://dktr0.github.io/cybernetic-samples/sounds/sounds.json");
  api.insertSound("samples/tink/000_tink1.wav","apitest",0);
  // api.deleteSound("apitest",0);
  // api.appendSound("samples/tink/000_tink1.wav","apitest");
  api.exolang("newpunctual","https://dktr0.github.io/Punctual/punctual.js");
  }

