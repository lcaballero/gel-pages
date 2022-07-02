console.log("site.js");

document.addEventListener("DOMContentLoaded", function() {
   var btns = document.querySelectorAll(".btn")
   var clipboard = new ClipboardJS(btns);
   clipboard.on('success', function(e) {
      console.info('Action:', e.action);
   });
});

