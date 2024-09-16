function toggleColor(id) {
    var background = document.getElementById(id).style.backgroundColor;
    if (background == "rgb(219, 219, 219)") {
        document.getElementById(id).style.background = "rgb(255, 255, 255)";
    } else {
        document.getElementById(id).style.background = "rgb(219, 219, 219)";
    }
}
