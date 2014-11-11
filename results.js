$(document).ready(function() {
  var tables = document.getElementById("Scores").children;
  var selectmenu = document.getElementById("selector");
  selectmenu.onchange = function() {
    var chosenOption=this.options[this.selectedIndex].value;
    for(i=0;i<tables.length;i++) {
      if(chosenOption == tables.item(i).id) {
        tables.item(i).setAttribute("class", "shownTable");
      } else {
        tables.item(i).setAttribute("class", "hiddenTable");
      }
    }
  }
  selectmenu.onchange();
  selectmenu.selectedIndex = 0;
  $(".benchmark").click(function () {
      // Show the benchmark sources.
    });
  $(".eCell").click(function () {
      $(".eMsg").hide();
      var errorBox = document.getElementById('div-'+this.id);
      errorBox.style.display = "inline";
      errorBox.style.top = parseInt($(this).position().top)+'px';
      errorBox.style.left = parseInt($(this).position().left)+'px';
    });
  $(".eMsg").click(function () {
      this.style.display = "none";
    });
});
