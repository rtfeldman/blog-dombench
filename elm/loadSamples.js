var ENV = {};
ENV.rows = 100;
ENV.timeout = 0;

var state = {databases: {}};
var loadCount = 0;

function getData() {
  // generate some dummy data
  data = {
    start_at: new Date().getTime() / 1000,
    databases: {}
  };

  for (var i = 1; i <= ENV.rows; i++) {
    data.databases["cluster" + i] = {
      queries: []
    };

    data.databases["cluster" + i + "slave"] = {
      queries: []
    };
  }

  Object.keys(data.databases).forEach(function(dbname) {
    var info = data.databases[dbname];

    var r = Math.floor((Math.random() * 10) + 1);
    for (var i = 0; i < r; i++) {
      var q = {
        canvas_action: null,
        canvas_context_id: null,
        canvas_controller: null,
        canvas_hostname: null,
        canvas_job_tag: null,
        canvas_pid: null,
        elapsed: Math.random() * 15,
        query: "SELECT blah FROM something",
        waiting: Math.random() < 0.5
      };

      if (Math.random() < 0.2) {
        q.query = "<IDLE> in transaction";
      }

      if (Math.random() < 0.1) {
        q.query = "vacuum";
      }

      info.queries.push(q);
    }

    info.queries = info.queries.sort(function(a, b) {
      return b.elapsed - a.elapsed;
    });
  });

  return data;
}

function loadSamples() {
  loadCount++;
  var newData = getData();

  Object.keys(newData.databases).forEach(function(dbname) {
    var sampleInfo = newData.databases[dbname];

    if (!state.databases[dbname]) {
      state.databases[dbname] = {
        name: dbname,
        samples: []
      }
    }

    var samples = state.databases[dbname].samples;
    samples.push({
      time: newData.start_at,
      queries: sampleInfo.queries
    });
    if (samples.length > 5) {
      samples.splice(0, samples.length - 5);
    }
  });

  setState(state);
  setTimeout(loadSamples, ENV.timeout);
}
