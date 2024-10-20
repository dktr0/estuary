"use strict";

// the only thing this exolang module needs to export is a function called exactly exoLang
// that provides an instance of an object that implements the exolang API (keep reading to 
// see what that looks like...)
export function exoLang() {
  return new ExoLangExample();
}

// this constructor for the exolang would be called by Estuary the first time the language is needed
function ExoLangExample() {
  console.log("new ExoLangExample constructed");
}

ExoLangExample.prototype.define = function(args) {
  // define can be implemented asynchronously:
  return new Promise(function (resolve,reject) {
    console.log("ExoLangExample received define (will respond 1 second later)");
    console.log(" zone: " + args.zone);
    console.log(" time: " + args.time);
    console.log(" text: " + args.text);
    setTimeout(function() {
      console.log("responding now to this code: " + args.text);
      if (args.text == "\ngoodcode\n") {
        resolve({ info: "some interesting text info about the evaluated code" });
      } else {
        reject(new Error("uhoh that wasn't goodcode"));
      }
    },1000);
  });
  
  // define can also be implemented synchronously:
  /*
  console.log("ExoLangExample received define");
  console.log(" zone: " + args.zone);
  console.log(" time: " + args.time);
  console.log(" text: " + args.text);
  if (args.text == "\ngoodcode\n") {
    return { info: "some interesting text info about the evaluated code" };
  }
  else {
    return new Error("uhoh that wasn't goodcode");
  }
  */

  // older synchronous model (deprecated)
  /*
  console.log("ExoLangExample received define");
  console.log(" zone: " + args.zone);
  console.log(" time: " + args.time);
  console.log(" text: " + args.text);
  if (args.text == "\ngoodcode\n") {
    return { success: true, info: "some interesting text info about the evaluated code" };
  }
  else {
    return { success: false, error: "uhoh that wasn't goodcode" };
  }
  */
}
  
ExoLangExample.prototype.clear = function(args) {
  console.log("ExoLangExample received clear");
  console.log(" zone: " + args.zone);
} // note: no return value is expected, and any that is provided will be ignored

ExoLangExample.prototype.preRender = function(args) {
  console.log("ExoLangExample received preRender");
  console.log(" canDraw: " + args.canDraw);
  console.log(" nowTime: " + args.nowTime);
  console.log(" previousDrawTime: " + args.previousDrawTime);
} // note: no return value is expected, and any that is provided will be ignored

ExoLangExample.prototype.render = function(args) {
  console.log("ExoLangExample received render");
  console.log(" zone: " + args.zone);
  console.log(" canDraw: " + args.canDraw);
  console.log(" nowTime: " + args.nowTime);
  console.log(" previousDrawTime: " + args.previousDrawTime);
  console.log(" windowStartTime: " + args.windowStartTime);
  console.log(" windowEndTime: " + args.windowEndTime);
  return []; // return value is a list of Estuary events (eg. for WebDirt, etc)
}

ExoLangExample.prototype.postRender = function(args) {
  console.log("ExoLangExample received postRender");
  console.log(" canDraw: " + args.canDraw);
  console.log(" nowTime: " + args.nowTime);
  console.log(" previousDrawTime: " + args.previousDrawTime);
} // note: no return value is expected, and any that is provided will be ignored

ExoLangExample.prototype.setTempo = function(args) {
  console.log("ExoLangExample received setTempo: " + args);
} // note: no return value is expected, and any that is provided will be ignored


