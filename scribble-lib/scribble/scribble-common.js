// Common functionality for PLT documentation pages

// Page Parameters ------------------------------------------------------------

var plt_root_as_query = false;

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
  if ((GetPageArgs().size === 0 || !a.dataset.pltdoc) && !plt_root_as_query) return;
  a.href = MergePageArgsIntoUrl(a.href);
}

function MergePageArgsIntoUrl(href) {
  const url = new URL(href, window.location.href);
  MergePageArgsIntoUrlObject(url);
  return url.href;
}

function MergePageArgsIntoUrlObject(url) {
  for (const [key, val] of GetPageArgs()) {
    if (key[0] == "q") continue; // use "q" to mean "don't propagate automatcially"
    if (url.searchParams.has(key)) continue;
    url.searchParams.append(key, val)
  }
  if (plt_root_as_query && !url.searchParams.has("PLT_Root")) {
      url.searchParams.append("PLT_Root", plt_root_as_query);
  }
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
    if (location.protocol == "file:") {
        // local storage or cookies are not going to work in modern browsers,
        // so add a query parameter to all URLs
        plt_root_as_query=root
    } else {
        SetCookie("PLT_Root."+ver, root);
    }
}

// adding index.html works because of the above
function GotoPLTRoot(ver, root_relative, here_to_root_relative) {
  // the relative path is optional, default goes to the toplevel start page
  if (!root_relative) root_relative = "index.html";
  if (here_to_root_relative == undefined) here_to_root_relative = "../"
  var famroot = false;
  if (root_relative == "index.html") {
    famroot = (GetPageArg("fam", false) ? GetPageArg("famroot", false) : false)
    if (famroot) {
      root_relative = famroot + "/index.html";
    }
  }

  var u = GetRootPath(ver);
  if (u == null) {
    if (famroot) {
      location = MergePageArgsIntoUrl(here_to_root_relative + famroot + "/index.html");
      return false;
    }
    // no cookie and no famroot => follow href, instead
    return true;
  }
  location = MergePageArgsIntoUrl(u + root_relative);
  return false;
}

function GetRootPath(ver) {
    var u = GetCookie("PLT_Root."+ver, null);
    if (u != null)
        return u;

    // via query argument? (especially for `file://` URLs)
    u = GetPageArg("PLT_Root", null)
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

AddOnLoad(function(){
  var es = document.getElementsByClassName("family-navigation");
  if (es.length > 0) {
    var fams = es[0].dataset.familynav.split(/,/);
    var fam = GetPageArg("famroot", false) && GetPageArg("fam", false);
    if (!fam) fam = "Racket";
    if (fams.indexOf(fam) == -1) {
      for (var i=0; i < es.length; i++) {
        es[i].style.display = "inline-block";
      }
    }
  }
});

AddOnLoad(function(){
  var es = document.getElementsByClassName("navfamily");
  for (var i=0; i < es.length; i++) {
    var e = es[i];
    if (e.dataset.fam != undefined) {
      var fams = e.dataset.fam.split(/,/);
      var fam = GetPageArg("fam", false);
      if (!fam) fam = "Racket";
      var link = document.createElement('a');
      var root = GetRootPath(e.dataset.version)
      var family_url;
      if (root == null) {
        family_url = new URL(e.dataset.famPath + "family/index.html", window.location.href);
      } else {
        family_url = new URL(root + "family/index.html", window.location.href);
      }
      family_url.searchParams.append("qfrom", window.location.href)
      MergePageArgsIntoUrlObject(family_url);
      if (fams.indexOf(fam) == -1) {
        var nav_as = document.createElement('div');
        link.textContent = "navigating as " + fam;
        link.href = family_url
        nav_as.appendChild(link)
        e.appendChild(nav_as)
      } else {
        var link = document.createElement('a');
        var span = e.children[0]
        link.textContent = span.textContent;
        link.href = family_url
        span.textContent = ''; // Clear span
        e.removeChild(span);
        link.appendChild(span);
        e.appendChild(link);
      }
    }
  }
});
