var startingState = localStorage.getItem("elm-planner-state") || "";

var elmPlanner = Elm.fullscreen(Elm.Planner.Main, {
    fileUpload: "",
    getStorage: startingState,
    ticker: false
});

elmPlanner.ports.focus.subscribe(function(nodeData) {
    var selector = nodeData[0];
    var content = nodeData[1];

    if (selector === "default") document.activeElement.blur()
    else if (selector.substring(0, 5) === "#node") setTimeout(function() {
        var nodes = document.querySelectorAll(selector);
        if (nodes.length === 1 && document.activeElement !== nodes[0]) {
            nodes[0].focus();
            nodes[0].value = content || nodes[0].value;
            nodes[0].select();
        }
    }, 20);
    else {
        var nodes = document.querySelectorAll(selector);
        if (nodes.length === 1 && document.activeElement !== nodes[0]) {
            nodes[0].focus();
            nodes[0].value = content || nodes[0].value;
            if (selector.substring(0, 6) === "#title") nodes[0].select();
        }
    }

});

elmPlanner.ports.save.subscribe(function(saveState) {
    if (navigator.msSaveBlob) {
        var blob_object = new Blob([saveState[1]], {
            type: "text/json;charset=utf-8"
        });
        return navigator.msSaveBlob(blob_object, saveState[0].trim() + ".elmplanner");
    } else {
        var pom = document.createElement('a');
        pom.setAttribute('href', 'data:text/json;charset=utf-8,' + encodeURIComponent(saveState[1]));
        pom.setAttribute('download', saveState[0].trim() + ".elmplanner");

        pom.style.display = 'none';
        document.body.appendChild(pom);

        pom.click();

        document.body.removeChild(pom);
    }
});

var loadButton = null;
var treePane = null;
var textBox = null;

function scrollToSelection(selector) {
    var elem;
    if (!selector)
        elem = document.getElementsByClassName("selected-focused")[0] || document.getElementsByClassName("selected-unfocused")[0];
    if (document.querySelectorAll(selector)[0] || elem)
        (document.querySelectorAll(selector)[0] || elem).scrollIntoViewIfNeeded();
    else setTimeout(scrollToSelection, 10);
}

function linkUpUI() {
    loadButton = document.getElementById("loadButton");
    treePane = document.getElementsByClassName("tree-pane")[0];
    textBox = document.getElementById("textbox");
    if (!loadButton || !treePane || !textBox) {
        setTimeout(linkUpUI, 50);
    } else {
        loadButton.addEventListener("change", function loadFile() {
            var reader = new FileReader();
            reader.onload = function(e) {
                if (e.target.result.trim() !== "") {
                    elmPlanner.ports.fileUpload.send(e.target.result.trim());
                    var wrapper = document.getElementById("loadWrapperForm");
                    if (wrapper) wrapper.firstChild.value = "";
                }

            };
            reader.readAsText(loadButton.files[0]);
        });

        elmPlanner.ports.ticker.send(true);

        Element.prototype.scrollIntoViewIfNeeded = function(centerIfNeeded) {
            centerIfNeeded = arguments.length === 0 ? true : !!centerIfNeeded;

            var parent = treePane,
                parentComputedStyle = window.getComputedStyle(parent, null),
                parentBorderTopWidth = parseInt(parentComputedStyle.getPropertyValue('border-top-width')),
                parentBorderLeftWidth = parseInt(parentComputedStyle.getPropertyValue('border-left-width')),
                overTop = this.offsetTop - parent.offsetTop < parent.scrollTop,
                overBottom = (this.offsetTop - parent.offsetTop + this.clientHeight - parentBorderTopWidth) > (parent.scrollTop + parent.clientHeight),
                overLeft = this.offsetLeft - parent.offsetLeft < parent.scrollLeft + 30,
                overRight = (this.offsetLeft - parent.offsetLeft + this.clientWidth - parentBorderLeftWidth) > (parent.scrollLeft + parent.clientWidth) * 1.4,
                alignWithTop = overTop && !overBottom;

            if ((overTop || overBottom) && centerIfNeeded) {
                parent.scrollTop = this.offsetTop - parent.offsetTop - parent.clientHeight / 2 - parentBorderTopWidth + this.clientHeight / 2;
            }

            if ((overLeft || overRight) && centerIfNeeded) {
                parent.scrollLeft = (this.offsetLeft - parent.offsetLeft - parent.clientWidth / 2 - parentBorderLeftWidth + this.offsetWidth / 2) - 30;
            }

            if ((overTop || overBottom || overLeft || overRight) && !centerIfNeeded) {
                this.scrollIntoView(alignWithTop);
            }
        };
    }
}

linkUpUI();
scrollToSelection();

elmPlanner.ports.setStorage.subscribe(function(stateJSON) {
    localStorage.setItem("elm-planner-state", stateJSON);
});

elmPlanner.ports.textBoxText.subscribe(function(text) {
    textBox.value = text;
});

elmPlanner.ports.log.subscribe(function(str) {
    console.log(str);
});

elmPlanner.ports.scroll.subscribe(function(selector) {
    scrollToSelection(selector);
});

document.onkeydown = function(e) {
    if (!e) var e = window.event;
    var keyCode = e.keyCode || e.which;
    if (keyCode == 9 || keyCode == 20) e.preventDefault();
    if ([37, 38, 39, 40].indexOf(keyCode) > -1 && [textBox, document.getElementById("title-bar-input")].indexOf(document.activeElement) === -1 && document.activeElement.id.substring(0, 4) !== "node") e.preventDefault(); // prevent arrow keys, space, tab
    if ([78, 79, 83].indexOf(keyCode) > -1 && (navigator.platform.match("Mac") ? e.metaKey : e.ctrlKey)) e.preventDefault();
    if (keyCode == 79 && e.ctrlKey) {
        e.preventDefault();
        loadButton.click();
    }
}
