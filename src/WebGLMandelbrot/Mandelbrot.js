"use strict";

// module WebGLMandelbrot.Mandelbrot

exports.drawMandelbrot = canvas => () => {
    var gl = canvas.getContext("webgl", {antialias: true})

    gl.clearColor(1.0, 1.0, 1.0, 1.0) //Red,Green,Blue,Alpha
    gl.clear(gl.COLOR_BUFFER_BIT)

    var buffer = gl.createBuffer()
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer)

    var vSource = [
        "precision mediump float;",
        "attribute vec2 vertex;",
        "void main(void) {",
        "gl_Position = vec4(vertex, 0.0, 1.0);",
        "}"
        ].join("\n")
    var vShader = gl.createShader(gl.VERTEX_SHADER)
    gl.shaderSource(vShader, vSource)
    gl.compileShader(vShader)
    gl.getShaderParameter(vShader, gl.COMPILE_STATUS)

    var fShader = gl.createShader(gl.FRAGMENT_SHADER)
    const fSource = `
    #define loopNum 100.0

    precision mediump float;

    uniform float t;
    uniform vec2 r;

    uniform vec2 offset;
    uniform float scale;

    const float PI = 3.141592653589793;

    float atanY(in float y){
        return atan(y, 1.0);
    }

    float mandelbrot(in vec2 c){
        vec2 z = vec2(0.0, 0.0);
        float n = 0.0;
        for(float i = 0.0; i < loopNum; i++){
            z = vec2(z.x * z.x - z.y * z.y + c.x, 2.0 * z.x * z.y + c.y);
            if(z.x * z.x + z.y * z.y > 4.0){
                n = i;
                break;
            }
        }
        return n;
    }

    void main(void){
        vec4 col = vec4(1.0,1.0,1.0,1.0);
        vec2 c = vec2((gl_FragCoord.x - r.x / 2.0) / scale + offset.x, (gl_FragCoord.y - r.y / 2.0) / scale + offset.y);
        float n = mandelbrot(c);

        if (n > 0.0){
            col = vec4(1.0 - n / loopNum, (1.0 - n / loopNum),1.0 - n / loopNum,1.0);
        }
        gl_FragColor = col;
    }
`
    gl.shaderSource(fShader, fSource)
    gl.compileShader(fShader)
    gl.getShaderParameter(fShader, gl.COMPILE_STATUS)

    var program = gl.createProgram()
    gl.attachShader(program, vShader)
    gl.attachShader(program, fShader)
    gl.linkProgram(program)
    gl.getProgramParameter(program, gl.LINK_STATUS)
    gl.useProgram(program)

    var vertex = gl.getAttribLocation(program, "vertex")
    gl.enableVertexAttribArray(vertex)
    gl.vertexAttribPointer(vertex, 2, gl.FLOAT, false, 0, 0)

    var vertices = [
        -1, -1,
        -1, 1,
        1, -1,
        1, 1
    ]
    var verticesNum = vertices.length / 2

    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.DYNAMIC_DRAW)

    var initTime = new Date().getTime()

    var scale =  canvas.width / 4
    var offset = {x: -0.746684956561724, y: -0.148407254991549}

    var isMouseHover = false
    var mousePosition = {x: 0, y: 0}

    const render = () => {
        gl.viewport(0, 0, canvas.width, canvas.height);

        gl.useProgram(program);

        var t = gl.getUniformLocation(program, "t")
        var r = gl.getUniformLocation(program, "r")
        var uniformOffset = gl.getUniformLocation(program, "offset")
        var uniformScale = gl.getUniformLocation(program, "scale")

        var time = (new Date().getTime() - initTime) * 0.001


        gl.uniform1f(t, time)
        gl.uniform2f(r, canvas.width, canvas.height)

        if(isMouseHover){
            if(mousePosition.x > canvas.width * 0.7){
                offset = {x: offset.x + 3 / scale, y: offset.y}
            }
            if(mousePosition.x < canvas.width * 0.3){
                offset = {x: offset.x - 3 / scale, y: offset.y}
            }
            if(mousePosition.y > canvas.height * 0.7){
                offset = {x: offset.x, y: offset.y - 3 / scale}
            }
            if(mousePosition.y < canvas.height * 0.3){
                offset = {x: offset.x, y: offset.y + 3 / scale}
            }
        }

        gl.uniform2f(uniformOffset, offset.x, offset.y)
        gl.uniform1f(uniformScale, scale)

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, verticesNum)
    }

    canvas.onmousewheel = function(event){
        if(event.wheelDelta > 0){
            scale *= 1.06
        }else{
            scale /= 1.06
        }
    }

    canvas.onmousemove = function(event){
        var rect = canvas.getBoundingClientRect()
        mousePosition.x = event.clientX - rect.x
        mousePosition.y = event.clientY - rect.y
    }

    canvas.onmouseover = function() {
        isMouseHover = true;
    }

    canvas.onmouseout = function() {
        isMouseHover = false;
    }

    setInterval(render, 1000 / 60);
}