
class EqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[0][i] == input2[0][i]) output[0][i] = 1; else output[0][i] = 0;
    }
    return true;
  }
}
registerProcessor('equal-processor',EqualProcessor);


class NotEqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[0][i] != input2[0][i]) output[0][i] = 1; else output[0][i] = 0;
    }
    return true;
  }
}
registerProcessor('notEqual-processor',NotEqualProcessor);


class GreaterThanProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[0][i] > input2[0][i]) output[0][i] = 1; else output[0][i] = 0;
    }
    return true;
  }
}
registerProcessor('greaterThan-processor',GreaterThanProcessor);


class GreaterThanOrEqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[0][i] >= input2[0][i]) output[0][i] = 1; else output[0][i] = 0;
    }
    return true;
  }
}
registerProcessor('greaterThanOrEqual-processor',GreaterThanOrEqualProcessor);


class LessThanProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[0][i] < input2[0][i]) output[0][i] = 1; else output[0][i] = 0;
    }
    return true;
  }
}
registerProcessor('lessThan-processor',LessThanProcessor);


class LessThanOrEqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[0][i] <= input2[0][i]) output[0][i] = 1; else output[0][i] = 0;
    }
    return true;
  }
}
registerProcessor('lessThanOrEqual-processor',LessThanOrEqualProcessor);

