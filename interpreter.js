Interpreter = function () {
  var isInt = function (value) {
    return !isNaN(value) && value == parseInt(value);
  };

  var isFloat = function (value) {
    return !isNaN(value) && value == parseFloat(value);
  };
  
  var isNumber = function (value) {
    return isInt(value) || isFloat(value);
  };

  var castType = function (value) {
    if (isFloat(value)) {
      return parseFloat(parseFloat(value).toFixed(2));
    }

    if (isInt(value)) {
      return parseInt(value);
    }

    if (value === 'true' || value === 'false') {
      return value === 'true';
    }

    return value;
  };

  var RuntimeError = function (message) {
    this.message = message;
  };

  var Parser = function (code) {
    var chars = code.split('');
    var tree = {
      chars: chars
    };

    var pushElement = function (node, start, end, type) {
      if (!node.elements) {
        node.elements = [];
      }

      var el = {
        start: start,
        end: end,
        content: castType(chars.slice(start, end).join(''))
      };

      if (type) {
        el.type = type;
      } else {
        if (isNumber(el.content)) {
          el.type = 'number';
        }
      }

      node.elements.push(el);
      return el;
    };

    var parse = function(element) {
      var startStatement = (element && element.start) || 0,
          endStatement = (element && element.end) || code.length,
          node = element || tree,
          openBrackets = 0,
          startBracket = 0,
          parseStart = 0,
          parseStatement = false,
          openQuote = false,
          startQuote = 0;

      for (var i = startStatement; i < endStatement; i++) {
        switch (chars[i]) {
          case '(':
            if (openBrackets == 0) {
              startBracket = i + 1;
            }

            openBrackets++;
            break;
          case ')':
            openBrackets--;

            if (openBrackets == 0) {
              parse(pushElement(node, startBracket, i));
            }

            break;
        }

        if (openBrackets != 0) {
          continue;
        }

        switch (chars[i]) {
          case '"':
            if (!openQuote) {
              openQuote = true;
              startQuote = i + 1;
            } else {
              openQuote = false;
              pushElement(node, startQuote, i, 'string');
            }
            break;
          case ")":
          case " ":
          case "\n":
          case "\t":
            if (parseStatement && !openQuote) {
              pushElement(node, parseStart, i);
            }

            parseStatement = false;
            break;
          default:
            if (!parseStatement && !openQuote) {
              parseStatement = true;
              parseStart = i;
            }
        }

        if (i == endStatement - 1 && parseStatement && !openQuote) {
          pushElement(node, parseStart, endStatement);
        }
      }

      if (openBrackets != 0) {
        throw new SyntaxError('Syntax error');
      }

      return tree;
    };

    return {
      parse: parse
    }
  };

  var Executor = function (tree, streams) {
    streams = streams || {};
    streams.output = '';

    var functions = {};
    var global = {};

    var print = function (string) {
      streams.output += string;
    };

    var makeArguments = function(parameters, operands, args, scope) {
      var newArgs = {};

      for (var attr in scope) {
        newArgs[attr] = scope[attr];
      }

      for (var attr2 in args) {
        newArgs[attr2] = args[attr2];
      }

      for (var j = 0; j < parameters.length; j++) {
        args[parameters[j]] = exec(operands[j], args);
      }

      return args;
    };

    var exec = function (node, args) {
      var elements = node.elements;
      var content = node.content;

      if (!elements) {
        if (node.type != 'number' && node.type != 'string') {
          if (args && typeof args[content] !== 'undefined') {
            return args[content];
          } else if (global && typeof global[content] !== 'undefined') {
            return global[content];
          } else {
            throw new RuntimeError('Undefined variable ' + content)
          }
        }

        return content;
      }

      if (elements.length == 1 && elements[0].type == 'string' || elements[0].type == 'number') {
        return elements[0].content;
      }

      var operator = elements[0];
      var operands = elements.slice(1);
      var result, j;

      if (operator.elements) {
        for (j = 0; j < elements.length; j++) {
          result = exec(elements[j], args);
        }
      } else if (operator.content.length && operator.content.indexOf('.') != -1) {
        var operatorParts = operator.content.split('.');

        switch (operatorParts[0]) {
          case 'Date':
            var date = new Date;
            result = date[operatorParts[1]].apply(date, operands.map(
            function (operand) {
              return exec(operand, args);
            }));
            break;
          case 'JSON':
          case 'document':
          case 'console':
            result = window[operatorParts[0]][operatorParts[1]].apply(window[operatorParts[0]], operands.map(
            function (operand) {
              return exec(operand, args);
            }));
            break;
          default:
            if (args[operatorParts[0]] || global[operatorParts[0]]) {
              var obj = args[operatorParts[0]] ? args[operatorParts[0]] : global[operatorParts[0]];

              result = obj[operatorParts[1]].apply(obj, operands.map(
              function (operand) {
                return exec(operand, args);
              }));
            } else {
              throw new RuntimeError('Undefined namespace ' + operatorParts[0]);
            }
        }
      } else {
        switch (operator.content) {
          case '+':
            result = 0;
            for (j = 0; j < operands.length; j++) {
              result += exec(operands[j], args);
            }
            break;
          case '-':
            result = exec(operands[0], args);
            for (j = 0; j < operands.length; j++) {
              if (j > 0) {
                result -= exec(operands[j], args);
              }
            }
            break;
          case '*':
            result = 1;
            for (j = 0; j < operands.length; j++) {
              result *= exec(operands[j], args);
            }
            break;
          case '/':
            result = exec(operands[0], args);
            for (j = 0; j < operands.length; j++) {
              if (j > 0) {
                result /= exec(operands[j], args);
              }
            }
            break;
          case '%':
            result = exec(operands[0], args) % exec(operands[1], args);
            break;
          case '>':
            result = exec(operands[0], args) > exec(operands[1], args);
            break;
          case '<':
            result = exec(operands[0], args) < exec(operands[1], args);
            break;
          case 'sqrt':
            result = Math.sqrt(exec(operands[0], args));
            break;
          case 'if':
            result = exec(operands[0], args)
              ? exec(operands[1], args)
              : (operands[2] ? exec(operands[2], args) : undefined);
            break;
          case 'print':
            for (j = 0; j < operands.length; j++) {
              print(exec(operands[j], args));
            }
            break;
          case 'null':
            result = null;
            break;
          case true:
          case false:
            result = operator.content;
            break;
          case '=':
            result = true;
            var first = exec(operands[0], args);
            for (j = 0; j < operands.length; j++) {
              result = !!(result && first == exec(operands[j], args));
            }
            break;
          case 'map':
            var newArgs = {};
            for (var attr in args) {
              newArgs[attr] = args[attr];
            }

            Array.prototype.map.call(exec(operands[0]), function (e) {
              newArgs.e = e;
              exec(operands[1], newArgs);
            });
            
            break;
          case 'alert':
          case 'atob':
          case 'btoa':
          case 'parseInt':
            result = window[operator.content](exec(operands[0], args));
            break;
          case 'RTCSessionDescription':
            result = new RTCSessionDescription(exec(operands[0], args));
            break;
          case 'RTCPeerConnection':
            var RTCPeerConnection = window.webkitRTCPeerConnection || window.RTCPeerConnection;
            result = new RTCPeerConnection(exec(operands[0], args));
            break;
          case 'addEventListener':
            var newArgs2 = {};
            for (var attr in args) {
              newArgs2[attr] = args[attr];
            }

            result = true;
            exec(operands[0], args).addEventListener(exec(operands[1], args), function(e) {
              newArgs2.e = e;
              return exec(operands[2], newArgs2);
            });
            break;
          case 'getProperty':
            result = exec(operands[0], args)[exec(operands[1], args)];
            break;
          case 'setPropertyStyle':
            result = exec(operands[2], args);
            exec(operands[0], args).style[operands[1].content] = result;
            break;
          case 'setProperty':
            result = exec(operands[2], args);
            exec(operands[0], args)[operands[1].content] = result;
            break;
          case 'var':
            if (global && global[operands[0].elements[0].content]) {
              result = global[operands[0].elements[0].content] = exec(operands[1], args);
            } else {
              result = args[operands[0].elements[0].content] = exec(operands[1], args);
            }

            break;
          case 'global':
            result = global[operands[0].elements[0].content] = exec(operands[1], args);
            break;
          case 'concatenate':
            result = operands.map(function (node) {
              return exec(node, args);
            }).join('');
            break;
          case 'hash':
            result = {};
            result[operands[0].content] = exec(operands[1], args);
            break;
          case 'fn':
            result = function () {
              var newArgs = args;
              for (var attr in arguments) {
                newArgs[String.fromCharCode(97 + parseInt(attr))] = arguments[attr];
              }
              return exec(operands[0], newArgs)
            };
            break;
          case 'array':
            result = [];
            for (j = 0; j < operands.length; j++) {
              result.push(exec(operands[j], args));
            }
            break;
          case 'define':
            var name = operands[0].elements[0].content;
            var parameters = operands[0].elements.slice(1).map(function (node) {
              return node.content;
            });
            functions[name] = {
              name: name,
              parameters: parameters,
              body: operands[1],
              scope: args
            };
            break;
          default:
            var fn = functions[operator.content];

            if (fn) {
              result = exec(fn.body, makeArguments(fn.parameters, operands, args, fn.scope));
            } else if (typeof args[operator.content] == 'function') {
              var argsArray = operands.map(function (node) {
                return exec(node, args);
              });
              result = args[operator.content].apply(window, argsArray);
            } else {
              throw new RuntimeError('Undefined operator ' + operator.content);
            }
        }
      }

      return castType(result);
    };

    /*exec = function (execOriginal) {
      var cache = {};

      return function(node, args) {
        var key = '' + node.content + JSON.stringify(args);

        if (cache[key]) {
          return cache[key];
        }

        return cache[key] = execOriginal(node, args);
      }
    }(exec);*/

    var execute = function () {
      var result, rootScope = {};

      if (tree.elements && tree.elements.length) {
        for (var i = 0; i < tree.elements.length; i++) {
          result = exec(tree.elements[i], rootScope);
        }
      }

      return result;
    };

    return {
      execute: execute
    };
  };

  var run = function (code, streams) {
    if (!streams) {
      streams = {};
    }

    try {
      var tree = Parser(code).parse();
      var x = Executor(tree, streams).execute();
      console.log(x);
      return x;
    } catch (error) {
      streams.error = error.message;
    }
  };

  return {
    run: run
  }
}();
