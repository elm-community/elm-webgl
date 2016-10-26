// eslint-disable-next-line no-unused-vars
var _elm_community$elm_webgl$Native_WebGL = function () {

  // setup logging
  // eslint-disable-next-line no-unused-vars
  function LOG(msg) {
    // console.log(msg);
  }

  var Utils = _elm_lang$core$Native_Utils;

  var rAF = typeof requestAnimationFrame !== 'undefined' ?
    requestAnimationFrame :
    function (cb) { setTimeout(cb, 1000 / 60); };

  function unsafeCoerceGLSL(src) {
    return { src: src };
  }

  function loadTexture(source) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      var img = new Image();
      img.onload = function () {
        return callback(_elm_lang$core$Native_Scheduler.succeed({ ctor: 'Texture', img: img }));
      };
      img.onerror = function () {
        return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Error' }));
      };
      img.crossOrigin = 'Anonymous';
      img.src = source;
    });
  }

  function loadTextureRaw(filter, source) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function (callback) {
      var img = new Image();
      img.onload = function () {
        return callback(_elm_lang$core$Native_Scheduler.succeed({ ctor: 'RawTexture', img: img }));
      };
      img.onerror = function () {
        return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Error' }));
      };
      img.crossOrigin = 'Anonymous';
      img.src = source;
    });
  }

  function textureSize(texture) {
    return Utils.Tuple2(texture.img.width, texture.img.height);
  }

  function render(vert, frag, buffer, uniforms, functionCalls) {

    if (!buffer.guid) {
      buffer.guid = Utils.guid();
    }

    return {
      vert: vert,
      frag: frag,
      buffer: buffer,
      uniforms: uniforms,
      functionCalls: functionCalls
    };

  }

  function do_texture(gl, texture) {

    var tex = gl.createTexture();
    LOG('Created texture');

    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, texture.img);
    switch (texture.ctor) {
      case 'Texture':
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
        break;
      case 'RawTexture':
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        break;
    }
    gl.generateMipmap(gl.TEXTURE_2D);
    //gl.bindTexture(gl.TEXTURE0, null);
    return tex;

  }

  function do_compile(gl, src, tipe) {

    var shader = gl.createShader(tipe);
    LOG('Created shader');

    gl.shaderSource(shader, src);
    gl.compileShader(shader);
    var compile = gl.COMPILE_STATUS;
    if (!gl.getShaderParameter(shader, compile)) {
      throw gl.getShaderInfoLog(shader);
    }

    return shader;

  }

  function do_link(gl, vshader, fshader) {

    var program = gl.createProgram();
    LOG('Created program');

    gl.attachShader(program, vshader);
    gl.attachShader(program, fshader);
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      throw gl.getProgramInfoLog(program);
    }

    return program;

  }

  function get_render_info(gl, render_type) {
    switch (render_type) {
      case 'Triangle':
        return { mode: gl.TRIANGLES, elemSize: 3, indexed: false };
      case 'LineStrip':
        return { mode: gl.LINE_STRIP, elemSize: 1, indexed: false };
      case 'LineLoop':
        return { mode: gl.LINE_LOOP, elemSize: 1, indexed: false };
      case 'Points':
        return { mode: gl.POINTS, elemSize: 1, indexed: false };
      case 'Lines':
        return { mode: gl.LINES, elemSize: 2, indexed: false };
      case 'TriangleStrip':
        return { mode: gl.TRIANGLE_STRIP, elemSize: 1, indexed: false };
      case 'TriangleFan':
        return { mode: gl.TRIANGLE_FAN, elemSize: 1, indexed: false };
      case 'IndexedTriangles':
        return { mode: gl.TRIANGLES, elemSize: 1, indexed: true };
    }
  }

  function get_attribute_info(gl, type) {
    switch (type) {
      case gl.FLOAT:
        return { size: 1, type: Float32Array, baseType: gl.FLOAT };
      case gl.FLOAT_VEC2:
        return { size: 2, type: Float32Array, baseType: gl.FLOAT };
      case gl.FLOAT_VEC3:
        return { size: 3, type: Float32Array, baseType: gl.FLOAT };
      case gl.FLOAT_VEC4:
        return { size: 4, type: Float32Array, baseType: gl.FLOAT };
      case gl.INT:
        return { size: 1, type: Int32Array, baseType: gl.INT };
      case gl.INT_VEC2:
        return { size: 2, type: Int32Array, baseType: gl.INT };
      case gl.INT_VEC3:
        return { size: 3, type: Int32Array, baseType: gl.INT };
      case gl.INT_VEC4:
        return { size: 4, type: Int32Array, baseType: gl.INT };
    }
  }

  /**
        Form the buffer for a given attribute.

        @param gl gl context
        @param attribute the attribute to bind to. We use its name to grab the record by name and also to know
                how many elements we need to grab.
        @param bufferElems The list coming in from elm.
        @param elem_length The length of the number of vertices that complete one 'thing' based on the drawing mode.
            ie, 2 for Lines, 3 for Triangles, etc.
  */
  function do_bind_attribute(gl, attribute, bufferElems, elem_length) {
    var idxKeys = [];
    for (var i = 0; i < elem_length; i++) {
      idxKeys.push('_' + i);
    }

    function dataFill(data, cnt, fillOffset, elem, key) {
      if (elem_length == 1) {
        for (var i = 0; i < cnt; i++) {
          data[fillOffset++] = cnt === 1 ? elem[key] : elem[key][i];
        }
      } else {
        idxKeys.forEach(function (idx) {
          for (var i = 0; i < cnt; i++) {
            data[fillOffset++] = (cnt === 1 ? elem[idx][key] : elem[idx][key][i]);
          }
        });
      }
    }

    var attributeInfo = get_attribute_info(gl, attribute.type);

    if (attributeInfo === undefined) {
      throw new Error('No info available for: ' + attribute.type);
    }

    var data_idx = 0;
    var array = new attributeInfo.type( _elm_lang$core$List$length(bufferElems) * attributeInfo.size * elem_length);

    A2(_elm_lang$core$List$map, function (elem) {
      dataFill(array, attributeInfo.size, data_idx, elem, attribute.name);
      data_idx += attributeInfo.size * elem_length;
    }, bufferElems);

    var buffer = gl.createBuffer();
    LOG('Created attribute buffer ' + attribute.name);

    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, array, gl.STATIC_DRAW);
    return buffer;
  }

  /**
    This sets up the binding cacheing buffers.

    We don't actually bind any buffers now except for the indices buffer, which we fill with 0..n. The problem
    with filling the buffers here is that it is possible to have a buffer shared between two webgl shaders; which
    could have different active attributes. If we bind it here against a particular program, we might not bind
    them all. That final bind is now done right before drawing.

    @param gl gl context
    @param bufferElems The list coming in from elm.
    @param elem_length The length of the number of vertices that complete one 'thing' based on the drawing mode.
            ie, 2 for Lines, 3 for Triangles, etc.
  */
  function do_bind_setup(gl, bufferElems, elem_length) {
    var buffers = {};

    var numIndices = elem_length * _elm_lang$core$List$length(bufferElems);
    var indices = new Uint16Array(numIndices);
    for (var i = 0; i < numIndices; i += 1) {
      indices[i] = i;
    }
    var indexBuffer = gl.createBuffer();
    LOG('Created index buffer');

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);

    var bufferObject = {
      numIndices: numIndices,
      indexBuffer: indexBuffer,
      buffers: buffers
    };

    return bufferObject;

  }

  function do_bind_setup_indexed(gl, indices) {
    var buffers = {};

    var numIndices = _elm_lang$core$List$length(indices);
    var indexBuffer = gl.createBuffer();
    LOG('Created index buffer');

    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
    var ind = [];
    A2(_elm_lang$core$List$map, function (elem) {
      ind.push(elem);
    }, indices);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, Uint16Array.from(ind), gl.STATIC_DRAW);

    var bufferObject = {
      numIndices: numIndices,
      indexBuffer: indexBuffer,
      buffers: buffers
    };

    return bufferObject;

  }

  function getProgID(vertID, fragID) {
    return vertID + '#' + fragID;
  }

  function drawGL(domNode, data) {

    var model = data.model;
    var gl = model.cache.gl;

    if (!gl) {
      return domNode;
    }

    gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    LOG('Drawing');

    function drawEntity(render) {
      if (_elm_lang$core$List$length(render.buffer._0) === 0) {
        return;
      }

      var progid;
      var program;
      if (render.vert.id && render.frag.id) {
        progid = getProgID(render.vert.id, render.frag.id);
        program = model.cache.programs[progid];
      }

      if (!program) {

        var vshader;
        if (render.vert.id) {
          vshader = model.cache.shaders[render.vert.id];
        } else {
          render.vert.id = Utils.guid();
        }

        if (!vshader) {
          vshader = do_compile(gl, render.vert.src, gl.VERTEX_SHADER);
          model.cache.shaders[render.vert.id] = vshader;
        }

        var fshader;
        if (render.frag.id) {
          fshader = model.cache.shaders[render.frag.id];
        } else {
          render.frag.id = Utils.guid();
        }

        if (!fshader) {
          fshader = do_compile(gl, render.frag.src, gl.FRAGMENT_SHADER);
          model.cache.shaders[render.frag.id] = fshader;
        }

        program = do_link(gl, vshader, fshader);
        progid = getProgID(render.vert.id, render.frag.id);
        model.cache.programs[progid] = program;

      }

      gl.useProgram(program);

      progid = progid || getProgID(render.vert.id, render.frag.id);
      var setters = model.cache.uniformSetters[progid];
      if (!setters) {
        setters = createUniformSetters(gl, model, program);
        model.cache.uniformSetters[progid] = setters;
      }

      setUniforms(setters, render.uniforms);

      var renderType = get_render_info(gl, render.buffer.ctor);
      var buffer = model.cache.buffers[render.buffer.guid];

      if (!buffer) {
        if (renderType.indexed) {
          buffer = do_bind_setup_indexed(gl, render.buffer._0._1);
        } else {
          buffer = do_bind_setup(gl, render.buffer._0, renderType.elemSize);
        }
        model.cache.buffers[render.buffer.guid] = buffer;
      }

      var numIndices = buffer.numIndices;
      var indexBuffer = buffer.indexBuffer;
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);

      var numAttributes = gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES);

      for (var i = 0; i < numAttributes; i += 1) {
        var attribute = gl.getActiveAttrib(program, i);

        var attribLocation = gl.getAttribLocation(program, attribute.name);
        gl.enableVertexAttribArray(attribLocation);

        if (buffer.buffers[attribute.name] === undefined) {
          var bufferElems;
          if (renderType.indexed) {
            bufferElems = render.buffer._0._0, renderType.elemSize;
          } else {
            bufferElems = render.buffer._0, renderType.elemSize;
          }
          buffer.buffers[attribute.name] = do_bind_attribute(gl, attribute, bufferElems, renderType.elemSize);
        }
        var attributeBuffer = buffer.buffers[attribute.name];
        var attributeInfo = get_attribute_info(gl, attribute.type);

        A2(_elm_lang$core$List$map, function (functionCall) {
          functionCall(gl);
        }, render.functionCalls);

        gl.bindBuffer(gl.ARRAY_BUFFER, attributeBuffer);
        gl.vertexAttribPointer(attribLocation, attributeInfo.size, attributeInfo.baseType, false, 0, 0);
      }
      gl.drawElements(renderType.mode, numIndices, gl.UNSIGNED_SHORT, 0);

    }

    A2(_elm_lang$core$List$map, drawEntity, model.renderables);
    return domNode;
  }

  function createUniformSetters(gl, model, program) {

    var textureCounter = 0;
    function createUniformSetter(program, uniform) {
      var uniformLocation = gl.getUniformLocation(program, uniform.name);
      switch (uniform.type) {
        case gl.INT:
          return function (value) {
            gl.uniform1i(uniformLocation, value);
          };
        case gl.FLOAT:
          return function (value) {
            gl.uniform1f(uniformLocation, value);
          };
        case gl.FLOAT_VEC2:
          return function (value) {
            gl.uniform2fv(uniformLocation, value);
          };
        case gl.FLOAT_VEC3:
          return function (value) {
            gl.uniform3fv(uniformLocation, value);
          };
        case gl.FLOAT_VEC4:
          return function (value) {
            gl.uniform4fv(uniformLocation, value);
          };
        case gl.FLOAT_MAT4:
          return function (value) {
            gl.uniformMatrix4fv(uniformLocation, false, value);
          };
        case gl.SAMPLER_2D:
          var currentTexture = textureCounter;
          var activeName = 'TEXTURE' + currentTexture;
          textureCounter += 1;
          return function (value) {
            var texture = value;
            var tex = undefined;
            if (texture.id) {
              tex = model.cache.textures[texture.id];
            } else {
              texture.id = Utils.guid();
            }
            if (!tex) {
              tex = do_texture(gl, texture);
              model.cache.textures[texture.id] = tex;
            }
            gl.activeTexture(gl[activeName]);
            gl.bindTexture(gl.TEXTURE_2D, tex);
            gl.uniform1i(uniformLocation, currentTexture);
          };
        case gl.BOOL:
          return function (value) {
            gl.uniform1i(uniformLocation, value);
          };
        default:
          LOG('Unsupported uniform type: ' + uniform.type);
          return function () {};
      }
    }

    var uniformSetters = {};
    var numUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
    for (var i = 0; i < numUniforms; i += 1) {
      var uniform = gl.getActiveUniform(program, i);
      uniformSetters[uniform.name] = createUniformSetter(program, uniform);
    }

    return uniformSetters;
  }

  function setUniforms(setters, values) {
    Object.keys(values).forEach(function (name) {
      var setter = setters[name];
      if (setter) {
        setter(values[name]);
      }
    });
  }

  function enable(capability) {
    return function (gl) {
      gl.enable(gl[capability]);
    };
  }

  function disable(capability) {
    return function (gl) {
      gl.disable(gl[capability]);
    };
  }

  function blendColor(r, g, b, a) {
    return function (gl) {
      gl.blendColor(r, g, b, a);
    };
  }

  function blendEquation(mode) {
    return function (gl) {
      gl.blendEquation(gl[mode]);
    };
  }

  function blendEquationSeparate(modeRGB, modeAlpha) {
    return function (gl) {
      gl.blendEquationSeparate(gl[modeRGB], gl[modeAlpha]);
    };
  }

  function blendFunc(src, dst) {
    return function (gl) {
      gl.blendFunc(gl[src], gl[dst]);
    };
  }

  function depthFunc(mode) {
    return function (gl) {
      gl.depthFunc(gl[mode]);
    };
  }

  function sampleCoverage(value, invert) {
    return function (gl) {
      gl.sampleCoverage(value, invert);
    };
  }

  function stencilFunc(func, ref, mask) {
    return function (gl) {
      gl.stencilFunc(gl[func], ref, mask);
    };
  }

  function stencilFuncSeparate(face, func, ref, mask) {
    return function (gl) {
      gl.stencilFuncSeparate(gl[face], gl[func], ref, mask);
    };
  }

  function stencilOperation(fail, zfail, zpass) {
    return function (gl) {
      gl.stencilOp(gl[fail], gl[zfail], gl[zpass]);
    };
  }

  function stencilOperationSeparate(face, fail, zfail, zpass) {
    return function (gl) {
      gl.stencilOpSeparate(gl[face], gl[fail], gl[zfail], gl[zpass]);
    };
  }


  // VIRTUAL-DOM WIDGETS

  function toHtml(functionCalls, factList, renderables) {
    var model = {
      functionCalls: functionCalls,
      renderables: renderables,
      cache: {}
    };
    return _elm_lang$virtual_dom$Native_VirtualDom.custom(factList, model, implementation);
  }

  // WIDGET IMPLEMENTATION
  var implementation = {
    render: renderCanvas,
    diff: diff
  };


  function renderCanvas(model) {

    LOG('Render canvas');
    var canvas = document.createElement('canvas');
    var gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');

    if (gl) {
      A2(_elm_lang$core$List$map, function (functionCall) {
        functionCall(gl);
      }, model.functionCalls);
    } else {
      canvas = document.createElement('div');
      canvas.innerHTML = '<a href="http://get.webgl.org/">Enable WebGL</a> to see this content!';
    }

    model.cache = model.cache || {};
    model.cache.gl = gl;
    model.cache.shaders = [];
    model.cache.programs = {};
    model.cache.uniformSetters = {};
    model.cache.buffers = [];
    model.cache.textures = [];

    // Render for the first time.
    // This has to be done in animation frame,
    // because the canvas is not in the DOM yet,
    // when renderCanvas is called by virtual-dom
    rAF(function () {
      drawGL(canvas, {model: model});
    });

    return canvas;
  }


  function diff(oldModel, newModel) {
    newModel.model.cache = oldModel.model.cache;
    return {
      applyPatch: drawGL,
      data: newModel
    };
  }

  return {
    unsafeCoerceGLSL: unsafeCoerceGLSL,
    textureSize: textureSize,
    loadTexture: loadTexture,
    render: F5(render),
    toHtml: F3(toHtml),
    enable: enable,
    disable: disable,
    blendColor: F4(blendColor),
    blendEquation: blendEquation,
    blendEquationSeparate: F2(blendEquationSeparate),
    blendFunc: F2(blendFunc),
    depthFunc: depthFunc,
    sampleCoverage: F2(sampleCoverage),
    stencilFunc: F3(stencilFunc),
    stencilFuncSeparate: F4(stencilFuncSeparate),
    stencilOperation: F3(stencilOperation),
    stencilOperationSeparate: F4(stencilOperationSeparate),
    loadTextureRaw: F2(loadTextureRaw)
  };

}();

