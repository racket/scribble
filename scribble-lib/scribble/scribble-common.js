// Common functionality for PLT documentation pages

// Page Parameters ------------------------------------------------------------

function GetURL() {
  return new URL(location);
}

function GetPageArgs() {
  return GetURL().searchParams;
}

function GetPageQueryString() {
  return GetPageArgs().toString();
}

function GetPageArg(key, def) {
  return GetPageArgs().get(key) || def;
}

function MergePageArgsIntoLink(a) {
  if (GetPageArgs().size === 0 || !a.dataset.pltdoc) return;
  a.href = MergePageArgsIntoUrl(a.href);
}

function MergePageArgsIntoUrl(href) {
  const url = new URL(href, window.location.href);
  for (const [key, val] of GetPageArgs()) {
    if (url.searchParams.has(key)) continue;
    url.searchParams.append(key, val)
  }
  return url.href;
}

// Cookies --------------------------------------------------------------------

// Actually, try localStorage (a la HTML 5), first.

function GetCookie(key, def) {
  try {
    var v = localStorage[key];
    if (!v) v = def;
    return v;
  } catch (e) {
    var i, cookiestrs;
    try {
      if (document.cookie.length <= 0) return def;
      cookiestrs = document.cookie.split(/; */);
    } catch (e) { return def; }
    for (i = 0; i < cookiestrs.length; i++) {
      var cur = cookiestrs[i];
      var eql = cur.indexOf('=');
      if (eql >= 0 && cur.substring(0,eql) == key)
        return unescape(cur.substring(eql+1));
    }
    return def;
  }
}

function SetCookie(key, val) {
  try {
    localStorage[key] = val;
  } catch(e) {
    var d = new Date();
    d.setTime(d.getTime()+(365*24*60*60*1000));
    try {
      document.cookie =
        key + "=" + escape(val) + "; expires="+ d.toGMTString() + "; path=/";
    } catch (e) {}
  }
}

// note that this always stores a directory name, ending with a "/"
function SetPLTRoot(ver, relative) {
  var root = location.protocol + "//" + location.host
           + NormalizePath(location.pathname.replace(/[^\/]*$/, relative));
  SetCookie("PLT_Root."+ver, root);
}

// adding index.html works because of the above
function GotoPLTRoot(ver, relative) {
  var u = GetRootPath(ver);
  if (u == null) return true; // no cookie: use plain up link
  // the relative path is optional, default goes to the toplevel start page
  if (!relative) relative = "index.html";
  location = u + relative;
  return false;
}

function GetRootPath(ver) {
    var u = GetCookie("PLT_Root."+ver, null);
    if (u != null)
        return u;
    // use root specified by local-redirect wrapper, if present
    if (typeof user_doc_root != "undefined")
        return user_doc_root;
    return null;
}

// Utilities ------------------------------------------------------------------

var normalize_rxs = [/\/\/+/g, /\/\.(\/|$)/, /\/[^\/]*\/\.\.(\/|$)/];
function NormalizePath(path) {
  var tmp, i;
  for (i = 0; i < normalize_rxs.length; i++)
    while ((tmp = path.replace(normalize_rxs[i], "/")) != path) path = tmp;
  return path;
}

// `noscript' is problematic in some browsers (always renders as a
// block), use this hack instead (does not always work!)
// document.write("<style>mynoscript { display:none; }</style>");

// Interactions ---------------------------------------------------------------

function DoSearchKey(event, field, ver, top_path) {
  var val = field.value;
  if (event && event.key === 'Enter') {
    var u = GetRootPath(ver);
    if (u == null) u = top_path; // default: go to the top path
    u += "search/index.html?q=" + encodeURIComponent(val);
    u = MergePageArgsIntoUrl(u);
    location = u;
    return false;
  }
  return true;
}

function TocviewToggle(glyph, id) {
  var s = document.getElementById(id).style;
  var expand = s.display == "none";
  s.display = expand ? "block" : "none";
  glyph.innerHTML = expand ? "&#9660;" : "&#9658;";
}

function TocsetToggle() {
  document.body.classList.toggle("tocsetoverlay");
}

// Page Init ------------------------------------------------------------------

// Note: could make a function that inspects and uses window.onload to chain to
// a previous one, but this file needs to be required first anyway, since it
// contains utilities for all other files.
var on_load_funcs = [];
function AddOnLoad(fun) { on_load_funcs.push(fun); }
window.onload = function() {
  for (var i=0; i<on_load_funcs.length; i++) on_load_funcs[i]();
};

AddOnLoad(function(){
    var links = document.getElementsByTagName("a");
    for (var i=0; i<links.length; i++) MergePageArgsIntoLink(links[i]);
    var label = GetPageArg("ctxtname",false);
    if (!label) return;
    var indicator = document.getElementById("contextindicator");
    if (!indicator) return;
    indicator.innerHTML = label;
    indicator.style.display = "block";
  });

// Pressing "S" or "s" focuses on the "...search manuals..." text field
AddOnLoad(function(){
  window.addEventListener("keyup", function(e) {
    if ((e.key === 's' || e.key === 'S') && e.target === document.body) {
      var searchBox = document.getElementById('searchbox');
      if (searchBox) {
        searchBox.focus();
      }
    }
  }, false);
});
