chrome.app.runtime.onLaunched.addListener(function() {
  chrome.app.window.create('demo.html', {
    'width': 400,
    'height': 500
  });
});
