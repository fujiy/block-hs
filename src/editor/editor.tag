// Editor

require('./editor.sass')
require('./stmt.tag')
require('./expr.tag')
require('./type.tag')
require('./info.tag')
require('./sample.tag')

import Mixin from './mixin.js'

import I from 'Block.Bridge'

<editor>
  <nav class="navbar navbar-dark bg-dark">
    <a class="navbar-brand" href="#">Block.hs</a>
  </nav>
  <div class='tab sidebar'>
    Expr:
    <p each={d in sampleExprs}><expr-sample data={d}/></p>
    Function:
    <p each={d in prelude}><sample data={d}/></p>
  </div>
  <div class='tab main bg-light' ref='area'>
    <main-module data={main_module} lib={prelude}/>
  </div>

  <script>
    // this.mixin(Mixin.Data)
    this.mixin(Mixin.Trash)


    this.main_module = I.main_module
    this.prelude     = I.prelude
    this.sampleExprs = I.sampleExprs
    console.log(I, this.main_module, this.prelude);

    this.onrenew = d => {
        this.main_module = I.typeChecks(this.prelude)(d)
        console.log('main', this.main_module);
        this.tags['main-module'].update({data: this.main_module, lib: this.prelude})
    }
  </script>
</editor>

<main-module>
  <div ref="list">
    <statement each={stmt, i in data} data={stmt} index={i} renew={renewS(i)}/>
  </div>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Sortable)
    this.name = 'module'

    this.listData = this.data

    this.onsort = newData => {
        console.log(newData);
    }
    this.renewS = i => d => {
        console.log('renew', i, d);
        const r = I.typeCheck(opts.lib)(this.data)(d)
        // console.log(r);
        if (r.updated) this.renew(I.renewI(i)(r.s)(this.data))
        else {
            console.log('update');
            this.data[i] = r.s
            this.update()
        }
    }
  </script>
</main-module>
