var EstuaryIcons = (function() {
  var MAX_COLOR_VALUE = 255;
  var CENTER_VARIATION_RADIUS = 0.03125; // % of width or height for x or y resp
  var PADDING = 0.03125; // % of width or height
  var INNER_PADDING = 0.00625; // % of width or height
  var STROKE_RANGE_HIGH = 5.5; // px
  var STROKE_RANGE_LOW = 0.5;

  function interpolate(start, end, percentage) {
    return ((end - start) * percentage) + start;
  }

  /**
   * @param {DOMRect} bboxStart
   * @param {DOMRect} bboxEnd
   * @param {number} percentage
   */
  function doTransitionFrame(bboxStart, bboxEnd, percentage, element) {
    // TODO since using transform, might be better to calculate directly.
    var top = interpolate(bboxStart.top, bboxEnd.top, percentage);
    var left = interpolate(bboxStart.left, bboxEnd.left, percentage);
    var width = interpolate(bboxStart.width, bboxEnd.width, percentage);
    var height = interpolate(bboxStart.height, bboxEnd.height, percentage);

    element.style.transformOrigin = '0 0';
    // translate must go first or it throws off the origin in chrome...
    element.style.transform = 'translate(' + (left - bboxEnd.left) + 'px, ' + (top - bboxEnd.top) + 'px)' 
      + ' scale(' +  width / bboxEnd.width + ', ' + height / bboxEnd.height + ')';
  }

  /* export */ EstuaryIcons.snapshotPosition = snapshotPosition;
  function snapshotPosition(display) {
    var canvas = display.canvas;

    var bbox = canvas.getBoundingClientRect();
    // If width and height is 0, it is not mounted or not visible.
    if (bbox.width === 0 && bbox.height === 0)
      return null;

    return {
      top: bbox.top, left: bbox.left,
      width: bbox.width, height: bbox.height
    };
  }

  /* export */ EstuaryIcons.animateFrom = animateFrom;
  function animateFrom(display, startPos, duration) {
    var endPos = snapshotPosition(display);

    display.animation = {
      startPos: startPos,
      endPos: endPos,
      duration: duration
    };
  }

  /**
   * @typedef {Object} Display
   * @property {HTMLCanvasElement} canvas
   * @property {CanvasRenderingContext2D} ctx
   * @property {Animation} [animation]
   */

  /**
   * @typedef {Object} Animation
   * @property {DOMRect} startPos
   * @property {DOMRect} endPos
   * @property {number} duration - in seconds
   * @property {number} [startTime] - in seconds
   */

  function EstuaryIcons(width, height, options) {
    options = options || {};

    this.running = false;

    /** @member {Display[]} */
    this.displays = [];

    this.renderStartTime = NaN;
    this.lineCount = 0;

    this.linesPerSec = options.linesPerSec == null ? 15 : options.linesPerSec;
    this.secUntilFullRadius = options.secUntilFullRadius == null ? 10 : options.secUntilFullRadius; // 10s to reach full radius
    this.width = width;
    this.height = height;
  };

  EstuaryIcons.prototype.startRendering = function startRendering() {
    if (!this.running) {
      this.running = true;
      this.renderStartTime = performance.now();
      this.lastRenderTime = this.renderStartTime;
      requestAnimationFrame(this.renderFrame.bind(this));
    }
  }

  EstuaryIcons.prototype.addDisplay = function addDisplay(canvas) {
    if (canvas == null)
      canvas = document.createElement('canvas');

    canvas.classList.add('estuary-icon-display');

    canvas.width = this.width;
    canvas.height = this.height;

    var display = {
      canvas: canvas,
      ctx: canvas.getContext('2d')
    };

    this.displays.push(display);

    return display;
  }

  EstuaryIcons.prototype.renderFrame = function renderFrame(now) {
    var timeSinceStart = (now - this.renderStartTime) / 1000;
    // TODO limit to 60fps to allow for GC in between

    var numLinesNow = Math.max(0, Math.floor(timeSinceStart * this.linesPerSec));
    var numLinesToDraw = Math.min(numLinesNow - this.lineCount, 100); // Limit this incase it gets put in the background for a day

    this.lineCount = numLinesNow;

    var width = this.width;
    var height = this.height;

    var radMaxX = (width / 2) - (PADDING * (width / 2));
    var radMinX = INNER_PADDING * (width / 2);
    var radMaxY = (height / 2) - (PADDING * (height / 2));
    var radMinY = INNER_PADDING * (width / 2);

    var centerVariaitonX = CENTER_VARIATION_RADIUS * (width / 2);
    var centerVariaitonY = CENTER_VARIATION_RADIUS * (height / 2);

    var displays = this.displays;

    for (var line = 0; line < numLinesToDraw; line++) {
      var maxRadiusPercent = Math.min(timeSinceStart / this.secUntilFullRadius, 1.0)
      
      var r = Math.random() * MAX_COLOR_VALUE;
      var g = Math.random() * MAX_COLOR_VALUE;
      var b = Math.random() * MAX_COLOR_VALUE;
      var a = Math.random() * 0.8 + 0.2;
      var strokeColor = 'rgba(' + r + ', ' + g + ', ' + b + ', ' + a + ')';

      var lineWidth = (Math.random() * (STROKE_RANGE_HIGH - STROKE_RANGE_LOW)) + STROKE_RANGE_LOW;

      var radX = (Math.random() * (radMaxX - radMinX)) + radMinX;
      radX *= maxRadiusPercent; // Expand slowly
      var radY = (Math.random() * (radMaxY - radMinY)) + radMinY;
      radY *= maxRadiusPercent; // Expand slowly
      
      var x = (Math.random() * centerVariaitonX) + (width / 2);
      var y = (Math.random() * centerVariaitonY) + (height / 2);

      var arcStartAngle = Math.random() * Math.PI * 2;
      var arcEndAngle = arcStartAngle + Math.random() * Math.PI * 2;

      for (var i = 0, len = displays.length; i < len; i++) {
        var display = displays[i];
        var ctx = display.ctx;

        ctx.save();
        ctx.globalCompositeOperation = 'destination-out';
        ctx.fillStyle = 'rgba(0, 0, 0, 0.02)';
        ctx.fillRect(0, 0, width, height);    
        ctx.restore();

        ctx.lineCap = 'round';
        ctx.lineWidth = lineWidth
        ctx.strokeStyle = strokeColor;

        ctx.beginPath();
        ctx.ellipse(x, y, radX, radY, 0, arcStartAngle, arcEndAngle);
        ctx.stroke();
      }
    }

    this.renderAnimation(now);

    if (this.running)
      requestAnimationFrame(this.renderFrame.bind(this));
  }

  EstuaryIcons.prototype.renderAnimation = function renderAnimation(now) {
    var displays = this.displays;
    for (var i = 0, len = displays.length; i < len; i++) {
      var display = displays[i];

      var animation = display.animation;
      if (animation != null) {
        if (animation.endPos == null)
          // Wait until mounted as the snapshot will return null until
          animation.endPos = snapshotPosition(display);

        if (animation.endPos == null) {
          doTransitionFrame(animation.startPos, animation.startPos, 1, display.canvas);
          continue;
        }

        var startTime = animation.startTime;
        if (startTime == null)
          animation.startTime = startTime = now;

        var progress = (now - startTime) / (animation.duration * 1000);
        if (progress > 1) {
          delete display.animation;
          display.canvas.style.transform = '';
        } else {
          doTransitionFrame(animation.startPos, animation.endPos, progress, display.canvas);
        }
      }
    }
  }

  return EstuaryIcons;
})();