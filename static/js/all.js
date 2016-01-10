(function() {
  window.hideMessage = function() {
    var elm = document.getElementById("message");
    elm.parentNode.removeChild(elm);
  };
})()
