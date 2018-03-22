// Expr

import Mixin from './mixin.js'
import I from 'Block.Bridge'

// Bind ------------------------------------------------------------------------

<bind>
  <bind-left data={opts.data.value0}/>
  <span class='token'>=</span>
  <expr data={opts.data.value1} outer={true}/>
</bind>

<bind-left class='var'>
  <div class='term var {args.length == 0 ?"right":""}'>
    <span class='token'>{var.value0.value0}</span>
  </div>
  <pattern each={d, i in args} data={d} right={i == args.length - 1}/>

  <type-info show={hover} data={var.value1}/>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.mixin(Mixin.Selectable)

    this.args = I.appToArray(this.opts.data)
    this.var = this.args.shift()
  </script>
</bind-left>

// Expr ------------------------------------------------------------------------

<expr class={left: opts.left, right: opts.right, outer: outer}>
  <var-expr if={cons == 'var'} data={expr}/>
  <app-expr if={cons == 'app'} data={expr} spine={spine} outer={outer}/>
  <num-expr if={cons == 'num'} data={expr}/>
  <type     if={cons == 'emp'} data={scheme.value1}/>

  <type-info show={hover} data={scheme}/>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.mixin(Mixin.Selectable)

    this.expr   = opts.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = opts.data.value1
    this.spine  = opts.spine || ""
    this.outer  = opts.outer || (this.cons == 'app' && !opts.spine)

    if (this.cons == 'app' && !this.spine) {
        let es = I.appToArray(opts.data)
        this.spine = I.econs(es[0].value0)
    }
  </script>
</expr>

<highlight class={outer: opts.outer, hover: opts.hover}>

</highlight>

<var-expr class='term var'>
  <span class='token'>{opts.data.value0}</span>
</var-expr>

<app-expr class='{opts.spine} {opts.outer?"outer":""}'>
  <expr data={opts.data.value0} spine={opts.spine} left={true}/>
  <expr data={opts.data.value1} right={opts.outer}/>
</app-expr>

<num-expr class='term num'>
  <span class='token'>{opts.data.value0}</span>
</num-expr>

// Pattern ---------------------------------------------------------------------

<pattern class={right: opts.right}>
  <var-pattern if={cons == 'var'} data={expr}/>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Selectable)

    this.expr   = opts.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = opts.data.value1
  </script>
</pattern>

<var-pattern class='term var'>
  <span class='token'>{opts.data.value0}</span>
</var-pattern>
