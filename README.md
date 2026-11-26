# Silk  [![Current Version](https://clojars.org/com.domkm/silk/latest-version.svg)](https://clojars.org/com.domkm/silk)

## Isomorphic Clojure[Script] Routing

### Design Goals and Solutions

#### Compatible with Clojure and ClojureScript

The core functionality of Silk in `domkm.silk` is fully compatible with both Clojure and ClojureScript and was designed to be so from the ground up.
Server-specific code for use with Ring is in `domkm.silk.serve`.
There is currently no browser-specific code, though there probably will be in the future.

#### Extensible Routes

An isomorphic routing library must be extensible for environment-specific constraints.
For example, on the server-side we may want a route to only respond to `GET` HTTP requests, while on the browser-side we want that same route to work even though there is no concept of incoming HTTP requests.

This is easy to do with Silk's core protocol, `domkm.silk/Pattern`, and is shown below.

#### Bidirectional Routes

Routes should be bidirectional (commonly referred to as named). If you are serving resources, the likelihood is that you are also providing links to other resources.
Your code and routes quickly become coupled and brittle without support for URL formation.
Unidirectional routes (like Compojure) will, eventually, break; bidirectional routes will not.

Silk routes are named and a specific route can be retrieved from a collection of routes by its name.

#### Decoupled Matching and Handling

This architectural principle is especially important in isomorphic applications.
Matching will probably be similar on the server-side and browser-side (except for extensions mentioned above) but handling will likely be completely different.

Names are not restricted to strings or keywords, so a name could *be* a handler function.
Or you could store handler functions elsewhere and look them up by route name.
Silk does not impose restrictions.

#### Data, Not Functions/Macros

Data structures can be generated, transformed, and analyzed at runtime and, perhaps more importantly, can be inspected and printed in meaningful ways.
Macro DSLs and function composition make these things very difficult.

Silk routes are data structures.
They are not nested functions and are not created with complex macros.
They are easy to create, manipulate, and inspect from Clojure and ClojureScript.

#### Reasonably Convenient

Silk has a few special rules to make route definition and use fairly terse.
However, since routes are just data and are therefore easy to create and compose, users can easily define more convenient syntax for their specific use cases.

#### Reasonably Performant

This goal is not yet met. Well, it may be, but there are no benchmarks yet.

### Use

##### Take a few minutes to learn Silk

Patterns can be matched and unmatched with `domkm.silk/match` and `domkm.silk/unmatch` respectively.

Strings only match themselves.

```clojure
(silk/match "foo" "foo")
;=> {}
(silk/unmatch "foo" {})
;=> "foo"
```

Keywords are wildcards.

```clojure
(silk/match :foo "bar")
;=> {:foo "bar"}
(silk/unmatch :foo {:foo "bar"})
;=> "bar"
```

There are also built in patterns for common use cases.

```clojure
(silk/match (silk/int :answer) "42")
;=> {:answer 42}
(silk/unmatch (silk/int :answer) {:answer 42})
;=> "42"

(silk/match (silk/uuid :id) "c11902f0-21b6-4645-a218-9fa40ef69333")
;=> {:id #uuid "c11902f0-21b6-4645-a218-9fa40ef69333"}
(silk/unmatch (silk/uuid :id) {:id #uuid "c11902f0-21b6-4645-a218-9fa40ef69333"})
;=> "c11902f0-21b6-4645-a218-9fa40ef69333"

(silk/match (silk/cat "user-" (silk/int :id)) "user-42")
;=> {:id 42}
(silk/unmatch (silk/cat "user-" (silk/int :id)) {:id 42})
;=> "user-42"

(silk/match (silk/? :this {:this "that"}) "foo")
;=> {:this "foo"}
(silk/match (silk/? :this {:this "that"}) nil)
;=> {:this "that"}
```

Patterns can be data structures.

```clojure
(silk/match ["users" (silk/int :id)] ["users" "42"])
;=> {:id 42}
```
A route can be created with a 2-tuple. The first element is a route name and the second element is something that can be turned into a URL pattern.
If the second element is a vector, the first and second elements are `assoc`iated into the third element under `:path` and `:query` keys respectively.
If the second element is a map, it is left unchanged.

```clojure
(silk/url-pattern [["users" "list"] {"filter" :filter "limit" :limit} {:scheme "https"}])
;=> {:path ["users" "list"], :query {"filter" :filter, "limit" :limit}, :scheme "https"}

(silk/url-pattern {:path ["users" "list"] :query {"filter" :filter "limit" :limit} :scheme "https"})
;=> {:path ["users" "list"], :query {"filter" :filter, "limit" :limit}, :scheme "https"}

(silk/route [:route-name [["users" "list"] {"filter" :filter "limit" :limit} {:scheme "https"}]])
;=> #<Route domkm.silk.Route@6ebe4324>
```

Routes are patterns.

```clojure
(silk/match (silk/route [:route-name [["users" :username]]]) {:path ["users" "domkm"]})
;=> {:username "domkm", :domkm.silk/name :route-name, :domkm.silk/pattern {:path ["users" :username]}}
(silk/unmatch (silk/route [:route-name [["users" :username]]]) {:username "domkm"})
;=> #domkm.silk.URL{:scheme nil, :user nil, :host nil, :port nil, :path ["users" "domkm"], :query nil, :fragment nil}
```

None of that is particularly useful unless you can match and unmatch route collections. Fortunately, a collection of routes is also a pattern.

```clojure
(def user-routes
  (silk/routes [[:users-index [["users"]]]
                [:users-show [["users" (silk/int :id)]]]]))

(silk/match user-routes {:path ["users" "42"]})
;=> {:id 42, :domkm.silk/name :users-show, :domkm.silk/routes #<Routes domkm.silk.Routes@c6f8bbc>, ...}
(silk/unmatch user-routes {:id 42 :domkm.silk/name :users-show})
;=> #domkm.silk.URL{:scheme nil, :user nil, :host nil, :port nil, :path ["users" "42"], :query nil, :fragment nil}
```

If you don't care about the match order, you can create routes with a map.

```clojure
(def page-routes
  (silk/routes {:home-page [[]] ; match "/"
                :other-page [["pages" :title]]}))
```

Routes can be constrained by request methods. For route matching, an omitted or
`nil` `:query` is treated as `{}`. Optional query parameters can still supply defaults.

```clojure
(def api-routes
  (silk/routes {:api-data [["api"] {"limit" (silk/? (silk/int :limit) {:limit 100})
                                    "offset" (silk/? (silk/int :offset) {:offset 0})} (serve/POST)]}))


(silk/match api-routes {:path ["api"]})
;=> nil
(silk/match api-routes {:path ["api"] :request-method :post})
;=> {:limit 100, :offset 0, :domkm.silk/name :api-data, ...}
```

Routes can be combined.

```clojure
(def all-routes
  (silk/routes [user-routes
                page-routes
                [:comments [["comments"] {"id" (silk/uuid :id)}]]
                api-routes]))
```

__All matching and unmatching is pure and bidirectional.__

Matching and unmatching patterns is powerful and pure but quite verbose.
Silk provides a higher-level interface via `domkm.silk/arrive` and `domkm.silk/depart`

```clojure
(silk/arrive all-routes "/pages/about")
;=> {:title "about", :domkm.silk/name :other-page, ...}
```

You can also provide a handler function.

```clojure
(silk/arrive all-routes "/pages/about" :title)
;=> "about"
```

Unmatching is almost as easy.

```clojure
(silk/depart all-routes :other-page {:title "about"})
;=> "/pages/about"
```

Use `silk/?` to declare an optional query parameter. `depart` omits it when you do not supply a value.

```clojure
(def login-routes
  (silk/routes [[:login [["login"] {"returnUrl" (silk/? :return-url)}]]]))

(silk/depart login-routes :login)
;=> "/login"
(silk/depart login-routes :login {:return-url "/somewhere"})
;=> "/login?returnUrl=%2Fsomewhere"
```

Pass a handler as the fourth argument to `depart` to modify the generated URL.
The handler receives a `URL` record. For example, it can add a query parameter
that the route does not declare:

```clojure
(silk/depart login-routes :login {:return-url "/somewhere"}
  (fn [url] (str (update url :query assoc "source" "email"))))
;=> "/login?returnUrl=%2Fsomewhere&source=email"
```

`arrive` ignores undeclared query keys when checking a route, but an added key
can make an earlier route match instead. For example, an earlier `/login` route
that requires `source=email` would match the URL above before `:login`.

##### Repeated query parameters

A query key that occurs once has a string value. A key that repeats has a vector
of strings in URL order. Both `?tag` and `?tag=` supply an empty string for `tag`.
A plain capture like `{"tag" :tag}` keeps that value as-is. Query captures using
`silk/int`, `silk/bool`, `silk/uuid`, or `silk/regex` apply the pattern to each value.
They capture one result for a single occurrence, or a vector of results for repeats.
If any value fails the pattern, the route does not match. Use `silk/?` to allow
an absent key.

```clojure
(def item-routes
  (silk/routes [[:items [["items"] {"id" (silk/int :id)}]]]))

(silk/arrive item-routes "/items?id=2" :id)
;=> 2
(silk/arrive item-routes "/items?id=2&id=7" :id)
;=> [2 7]
(silk/arrive item-routes "/items?id=2&id=nope")
;=> nil
(silk/depart item-routes :items {:id [2 7]})
;=> "/items?id=2&id=7"
```

`depart` writes one query pair per repeated value, in order. A one-element vector
writes one pair, so `arrive` reads it back as a single value. An empty vector
omits a plain capture but is invalid for a typed capture. If a regex groups its
captures into a vector for one value, use a vector of those vectors for repeats.

__Go forth and route!__

### Tests

Install [Clojure CLI](https://clojure.org/guides/install_clojure), JDK 25, and Node.js 24 LTS. CI runs the specs on Clojure 1.12 and ClojureScript 1.12:

```sh
clojure -M:test:clj
clojure -M:test:cljs && node target/cljs-tests.js
```

Put shared specs in `.cljc` files under `spec/`. Both suites discover them automatically.

### Formatting

Run these commands from the repository root:

```sh
clojure -Srepro -M:fmt -fw
clojure -Srepro -M:fmt -fc
```

### License

Copyright &copy; 2014 Dom Kiva-Meyer

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
