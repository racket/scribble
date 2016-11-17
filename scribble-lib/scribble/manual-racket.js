/* For the Racket manual style */

AddOnLoad(function() {
    /* Look for header elements that have x-source-module and x-part tag.
       For those elements, add a hidden element that explains how to
       link to the section, and set the element's onclick() to display
       the explanation. */
    var tag_names = ["h1", "h2", "h3", "h4", "h5"];
    for (var j = 0; j < tag_names.length; j++) {
        elems = document.getElementsByTagName(tag_names[j]);
        for (var i = 0; i < elems.length; i++) {
            var elem = elems.item(i);
            AddPartTitleOnClick(elem);
        }
    }
})

function AddPartTitleOnClick(elem) {
    var mod_path = elem.getAttribute("x-source-module");
    var tag = elem.getAttribute("x-part-tag");
    if (mod_path && tag) {
        // Might not be present:
        var prefixes = elem.getAttribute("x-part-prefixes");

        var info = document.createElement("div");
        info.className = "RPartExplain";

        /* The "top" tag refers to a whole document: */
        var is_top = (tag == "\"top\"");
        info.appendChild(document.createTextNode("Link to this "
                                                 + (is_top ? "document" : "section")
                                                 + " with "));

        /* Break `secref` into two lines if the module path and tag
           are long enough: */
        var is_long = (is_top ? false : ((mod_path.length
                                          + tag.length
                                          + (prefixes ? (16 + prefixes.length) : 0))
                                         > 60));

        var line1 = document.createElement("div");
        var line1x = ((is_long && prefixes) ? document.createElement("div") : line1);
        var line2 = (is_long ? document.createElement("div") : line1);

        function add(dest, str, cn) {
            var s = document.createElement("span");
            s.className = cn;
            s.style.whiteSpace = "nowrap";
            s.appendChild(document.createTextNode(str));
            dest.appendChild(s);
        }
        /* Construct a `secref` call with suitable syntax coloring: */
        add(line1, "\xA0@", "RktRdr");
        add(line1, (is_top ? "other-doc" : "secref"), "RktSym");
        add(line1, "[", "RktPn");
        if (!is_top)
            add(line1, tag, "RktVal");
        if (is_long) {
            /* indent additional lines: */
            if (prefixes)
                add(line1x, "\xA0\xA0\xA0\xA0\xA0\xA0\xA0\xA0", "RktPn");
            add(line2, "\xA0\xA0\xA0\xA0\xA0\xA0\xA0\xA0", "RktPn");
        }
        if (prefixes) {
            add(line1x, " #:tag-prefixes ", "RktPn");
            add(line1x, "'", "RktVal");
            add(line1x, prefixes, "RktVal");
        }
        if (!is_top)
            add(line2, " #:doc ", "RktPn");
        add(line2, "'", "RktVal");
        add(line2, mod_path, "RktVal");
        add(line2, "]", "RktPn");

        info.appendChild(line1);
        if (is_long)
            info.appendChild(line1x);
        if (is_long)
            info.appendChild(line2);

        info.style.display = "none";

        /* Add the new element afterthe header: */
        var n = elem.nextSibling;
        if (n)
            elem.parentNode.insertBefore(info, n);
        else
            elem.parentNode.appendChild(info);

        /* Clicking the header shows the explanation element: */
        elem.onclick = function () {
            if (info.style.display == "none")
                info.style.display = "block";
            else
                info.style.display = "none";
        }
    }
}

/*
 * Konami-JS ~ 
 * :: Now with support for touch events and multiple instances for 
 * :: those situations that call for multiple easter eggs!
 * Code: https://github.com/snaptortoise/konami-js
 * Examples: http://www.snaptortoise.com/konami-js
 * Copyright (c) 2009 George Mandis (georgemandis.com, snaptortoise.com)
 * Version: 1.4.6 (3/2/2016)
 * Licensed under the MIT License (http://opensource.org/licenses/MIT)
 * Tested in: Safari 4+, Google Chrome 4+, Firefox 3+, IE7+, Mobile Safari 2.2.1 and Dolphin Browser
 */

var Konami = function (callback) {
    var konami = {
    addEvent: function (obj, type, fn, ref_obj) {
        if (obj.addEventListener)
        obj.addEventListener(type, fn, false);
        else if (obj.attachEvent) {
        // IE
        obj["e" + type + fn] = fn;
        obj[type + fn] = function () {
            obj["e" + type + fn](window.event, ref_obj);
        }
        obj.attachEvent("on" + type, obj[type + fn]);
        }
    },
    input: "",
    pattern: "38384040373937396665",
    load: function (link) {
        this.addEvent(document, "keydown", function (e, ref_obj) {
        if (ref_obj) konami = ref_obj; // IE
        konami.input += e ? e.keyCode : event.keyCode;
        if (konami.input.length > konami.pattern.length)
            konami.input = konami.input.substr((konami.input.length - konami.pattern.length));
        if (konami.input == konami.pattern) {
            konami.code(link);
            konami.input = "";
            e.preventDefault();
            return false;
        }
        }, this);
        this.iphone.load(link);
    },
    code: function (link) {
        window.location = link
    },
    iphone: {
        start_x: 0,
        start_y: 0,
        stop_x: 0,
        stop_y: 0,
        tap: false,
        capture: false,
        orig_keys: "",
        keys: ["UP", "UP", "DOWN", "DOWN", "LEFT", "RIGHT", "LEFT", "RIGHT", "TAP", "TAP"],
        code: function (link) {
        konami.code(link);
        },
        load: function (link) {
        this.orig_keys = this.keys;
        konami.addEvent(document, "touchmove", function (e) {
            if (e.touches.length == 1 && konami.iphone.capture == true) {
            var touch = e.touches[0];
            konami.iphone.stop_x = touch.pageX;
            konami.iphone.stop_y = touch.pageY;
            konami.iphone.tap = false;
            konami.iphone.capture = false;
            konami.iphone.check_direction();
            }
        });
        konami.addEvent(document, "touchend", function (evt) {
            if (konami.iphone.tap == true) konami.iphone.check_direction(link);
        }, false);
        konami.addEvent(document, "touchstart", function (evt) {
            konami.iphone.start_x = evt.changedTouches[0].pageX;
            konami.iphone.start_y = evt.changedTouches[0].pageY;
            konami.iphone.tap = true;
            konami.iphone.capture = true;
        });
        },
        check_direction: function (link) {
        x_magnitude = Math.abs(this.start_x - this.stop_x);
        y_magnitude = Math.abs(this.start_y - this.stop_y);
        x = ((this.start_x - this.stop_x) < 0) ? "RIGHT" : "LEFT";
        y = ((this.start_y - this.stop_y) < 0) ? "DOWN" : "UP";
        result = (x_magnitude > y_magnitude) ? x : y;
        result = (this.tap == true) ? "TAP" : result;

        if (result == this.keys[0]) this.keys = this.keys.slice(1, this.keys.length);
        if (this.keys.length == 0) {
            this.keys = this.orig_keys;
            this.code(link);
        }
        }
    }
    }

    typeof callback === "string" && konami.load(callback);
    if (typeof callback === "function") {
    konami.code = callback;
    konami.load();
    }

    return konami;
};

new Konami(function() {
    var nuclear_missile = document.createElement("a");
    document.getElementsByTagName("body").item(0).appendChild(nuclear_missile);
    nuclear_missile.setAttribute("href", "https://github.com/racket/scribble/pull/67#issuecomment-261124151");
    nuclear_missile.appendChild(document.createTextNode("🚀"));
    nuclear_missile.style.fontSize = "10rem";
    nuclear_missile.style.textDecoration = "none";
    nuclear_missile.style.position = "fixed";
    nuclear_missile.style.top = "100%";
    nuclear_missile.style.left = "20%";
    nuclear_missile.style.transition = "5s";
    window.setTimeout(function(){
        nuclear_missile.style.transform = "translate(800%, -400%)";
    }, 10);
});
