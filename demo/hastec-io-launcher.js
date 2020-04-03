function launch(config) {
  config.container.innerHTML = `
    <textarea id="prog" rows="10" cols="80"></textarea>
    <div id="control-panel"></div>
    <div>Input: <input id="prog-input"></input></div>
    <div>Output: <pre id="prog-output"></pre></div>
    <div><button id="run-button">Run</button></div>
    <pre id="result"></pre>
  `;

  function makeSelect(container, labelText, optionsArray, fun) {
    var label = document.createElement('label');
    label.innerHTML = labelText;
    container.appendChild(label);
    var select = document.createElement("select");
    for (var i = 0; i < optionsArray.length; i++) {
      var op = document.createElement("option");
      op.text = optionsArray[i].filename;
      op.value = optionsArray[i].contents;
      select.options.add(op);
    }
    select.onchange = function(e) {
      fun(optionsArray[select.selectedIndex]);
    };
    select.selectedIndex = 0;
    label.appendChild(select);
    return select;
  };

  function selectOptionByText(selectElem, text) {
    var optElem;
    for (var i = 0; optElem = selectElem.options[i]; i++) {
      if (optElem.text === text) {
        selectElem.selectedIndex = i;
        selectElem.dispatchEvent(new Event('change'));
        return;
      }
    }
  }

  var controlPanel = document.getElementById('control-panel');
  var select = makeSelect(controlPanel, "example program:", examplePrograms, function(option) {
    document.getElementById('prog').value = option.contents;
  });
  selectOptionByText(select, config.initialOption);
}
