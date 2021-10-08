"use strict";

// module WebGLMandelbrot.Mandelbrot

var scale;
var isShoot;

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

    scale =  canvas.width / 4
    var offset = {x: 0, y: 0}

    var isMouseHover = false
    var isMouseDown = false
    var mousePosition = {x: 0, y: 0}
    var mouseDownedPosition = {x: 0, y: 0}
    var mouseDownedOffset = {x: 0, y: 0}
    var prevOffset = {x: 0, y: 0}

    var scrollSpeed = {x: 0, y: 0}

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
/*
        if(isMouseHover){
            scrollSpeed.x = 0
            scrollSpeed.y = 0
            if(mousePosition.x > canvas.width * 0.8){
                scrollSpeed.x += (mousePosition.x - canvas.width * 0.8) * 0.04 / scale
            }
            if(mousePosition.x < canvas.width * 0.2){
                scrollSpeed.x += (mousePosition.x - canvas.width * 0.2) * 0.04 / scale
            }
            if(mousePosition.y > canvas.height * 0.8){
                scrollSpeed.y += (mousePosition.y - canvas.height * 0.8) * 0.04 / scale
            }
            if(mousePosition.y < canvas.height * 0.2){
                scrollSpeed.y += (mousePosition.y - canvas.height * 0.2) * 0.04 / scale
            }
            offset = {x: offset.x + scrollSpeed.x, y: offset.y + scrollSpeed.y}
        }
*/
        if(isMouseDown){
            offset.x = Math.max (-2, Math.min(2, mouseDownedOffset.x - (mousePosition.x - mouseDownedPosition.x) / scale))
            offset.y = Math.max (-2, Math.min(2, mouseDownedOffset.y - (mousePosition.y - mouseDownedPosition.y) / scale))
            scrollSpeed.x = offset.x - prevOffset.x
            scrollSpeed.y = offset.y - prevOffset.y
        }
        else
        {
            scrollSpeed.x = scrollSpeed.x / 1.05
            scrollSpeed.y = scrollSpeed.y / 1.05
            offset = {
                x: Math.max (-2, Math.min(2, offset.x + scrollSpeed.x)),
                y: Math.max (-2, Math.min(2, offset.y + scrollSpeed.y))
            }
        }

        gl.uniform2f(uniformOffset, offset.x, offset.y)
        gl.uniform1f(uniformScale, scale)

        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, verticesNum)

        prevOffset.x = offset.x
        prevOffset.y = offset.y

        gl.flush()

        if (isShoot) {
            let link = document.createElement("a");

            link.href = canvas.toDataURL("image/png");
            link.download = "mandelbrot.png";
            link.click();

            canvas.width = canvas.width / 2
            canvas.height = canvas.height / 2
            scale = scale / 2
            isShoot = false;
        }
    }

    canvas.onmousewheel = function(event){
        var xComplex = (mousePosition.x - canvas.width / 2) / scale + offset.x
        var yComplex = (mousePosition.y - canvas.height / 2) / scale + offset.y
        if(event.wheelDelta > 0 && scale < canvas.width * (Math.pow(2, 16))){
            scale *= 1.06
            offset.x = xComplex - (xComplex - offset.x) / 1.06
            offset.y = yComplex - (yComplex - offset.y) / 1.06
        }
        if(event.wheelDelta < 0 && scale > canvas.width / 8){
            scale /= 1.06
            offset.x = xComplex - (xComplex - offset.x) * 1.06
            offset.y = yComplex - (yComplex - offset.y) * 1.06
        }
    }

    canvas.onmousemove = function(event){
        var rect = canvas.getBoundingClientRect()
        mousePosition.x = event.clientX - rect.x
        mousePosition.y = canvas.height - (event.clientY - rect.y)
    }

    canvas.onmouseover = function() {
        isMouseHover = true;
    }

    canvas.onmouseout = function() {
        isMouseHover = false;
        isMouseDown = false;
    }

    canvas.onmousedown = function() {
        isMouseDown = true
        mouseDownedPosition.x = mousePosition.x
        mouseDownedPosition.y = mousePosition.y
        mouseDownedOffset.x = offset.x
        mouseDownedOffset.y = offset.y
    }

    canvas.onmouseup = function() {
        isMouseDown = false
    }

    setInterval(render, 1000 / 60);
}

exports.shoot = canvas => () => {
    canvas.width = canvas.width * 2
    canvas.height = canvas.height * 2
    scale = scale * 2

    isShoot = true
}