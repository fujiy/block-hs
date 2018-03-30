// Expr

import Mixin from './mixin.js'
import I from 'Block.Bridge'

// Bind ------------------------------------------------------------------------

<bind>
  <div>
    <bind-left data={data.value0} renew={renewL}/>
    <span class='token'>=</span>
  </div>
  <expr data={data.value1} outer={true}/>

  <script>
    this.mixin(Mixin.Data)
    this.renewL  = d => this.renew(I.renewLeft(d)(this.data))
    this.onrenew = d => this.renew(I.renewRight(d)(this.data))
  </script>
</bind>

<bind-left class='var'>
  <!-- <func-sample data={var}/>
  <div class='term var {args.length == 0 ?"right":""}'>
    <span class='token'>{var.value0.value0}</span>
</div> -->
  <bind-var data={var}/>
  <pattern each={d, i in args} data={d} right={i == args.length - 1} renew={renewA(i)}/>
  <add-area renew={addArg}/>

  <type-info show={hover} data={var.value1}/>
  <highlight outer={outer} hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.args = I.appToArray(this.opts.data)
    this.var  = this.args.shift()

    this.onrenew = d => this.renew(I.toApp(d)(this.args))
    this.renewA  = i => d => this.renew(d ? I.renewArgs(i)(d)(this.args)(this.var)
                                          : I.deleteArg(i)(this.args)(this.var) )
    this.addArg  = d => this.renew(I.toApp(this.var)(this.args.concat(d || I.pempty)))
  </script>
</bind-left>

<add-area onclick={add}>
  <div ref='slot' class='slot'></div>
  <span class='token'>+</span>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Droppable)
    this.add = () => this.renew(null)
  </script>
</add-area>

<bind-var class={func: func}>
  <div ref='slot' class='slot'>
    <div class='sample' if={cons == 'var'}>
      <div class='term var'>
        <handle/>
        <input-field data={expr.value0}/>
        <!--<span class='token'>{expr.value0}</span>-->
      </div>
      <hole each={t, i in holes} data={t} right={i == holes.length - 1} spine='var' conpact={false}/>
    </div>
    <div class='sample' if={cons == 'ope'}>
      <hole data={holes[0]} spine='opv' left={true} conpact={false}/>
      <div class='term opv'>
        <input-field data={expr.value0}/>
      </div>
      <hole data={holes[1]} spine='opv' right={true} conpact={false}/>
    </div>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Clonable)

    this.name   = 'expr'
    this.cons   = I.econs(this.data.value0)
    this.expr   = this.cons == 'ope' ? this.data.value0.value0.value0 : this.data.value0
    this.scheme = this.data.value1
    this.holes  = I.arrowToArray(this.scheme.value1)
    this.holes.pop()
    this.func   = this.holes.length > 0

    this.onrenew = s => this.renew(I.renewExpr(I.varC(s))(this.data))
  </script>
</bind-var>

// Expr ------------------------------------------------------------------------

<expr class={left: opts.left, right: opts.right, outer: outer, func: func,
             bracket: bracket, factor: factor, hole: opts.hole}>
  <div ref='slot' class='slot'>
    <expr-emp    if={cons == 'emp'} data={scheme.value1}/>
    <var-expr    if={cons == 'var'} data={expr}/>
    <app-expr    if={cons == 'app'} data={expr} spine={spine} outer={outer && !func}/>
    <expr-lambda if={cons == 'lam'} data={expr} renew={renewE}/>
    <expr-oper   if={cons == 'ope'} data={expr}/>
    <num-expr    if={cons == 'num'} data={expr}/>
    <expr-if     if={cons == 'ift'} data={expr}/>
    <expr-case   if={cons == 'cas'} data={expr}/>
  </div>
  <hole if={outer && func && !(factor && opts.left)} spine={spine}
        each={t, i in holes} data={t} right={i == holes.length - 1} renew={apply(i)}/>

  <infos show={hover} data={infos}/>
  <type-info show={hover} data={scheme}/>
  <highlight hover={hover} infos={infos}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Draggable)

    this.expr    = this.data.value0
    this.cons    = I.econs(this.expr)
    this.scheme  = this.data.value1
    this.infos   = this.data.value2
    this.holes   = I.arrowToArray(this.scheme.value1); this.holes.pop()
    this.oper    = this.cons == 'ope'
    this.func    = this.holes.length > 0 && !(this.oper && this.holes.length <= 2)
    this.app     = this.cons == 'app' && !opts.spine
    this.factor  = this.cons == 'lam' || this.cons == 'ift'
    // this.oper    = this.cons == 'ope'
    this.bracket = (opts.bracket || opts.hole) && (this.func || this.app || this.oper)
                || this.factor && (opts.left)
    this.outer   = opts.outer || this.bracket || this.app
    const spineC = I.econs(I.appToArray(this.data)[0].value0)
    this.spine   = opts.spine || (spineC == 'lam' || spineC == 'ift' ? 'bra' : spineC)

    this.name = 'expr'

    this.onrenew = d => this.renew(I.renewExpr(d)(this.data))
    this.renewE  = d => this.renew(d)
    this.apply = i => d => {
        console.log('apply', i, d);
        console.log(I.fillExprWith(i)(d)(this.data));
        this.renew(I.fillExprWith(i)(d)(this.data))
    }

    this.onremove = () => {
        console.log('remove');
        this.renew(I.eempty)
    }
    this.ondrop = d => this.renew(I.assignExpr(this.data)(d))
  </script>
</expr>

<hole class='{opts.spine} {opts.left?"left":""} {opts.right?"right":""} {conpact?"conpact":""}'>
  <div ref='slot' class='slot'>
    <type data={opts.data}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    if (opts.renew) this.mixin(Mixin.Droppable)

    this.name = 'expr'
    this.scheme  = I.spure(this.data)
    this.conpact = opts.conpact == null ? true : opts.conpact

    this.ondrop = d => this.renew(d)
  </script>
</hole>

<handle>
  <div class='grip'></div>
  <button if={opts.remove} onclick={opts.remove} class='remove'>×</button>
</handle>

<highlight class={hover: opts.hover, error: error}>
  <script>
    this.error = opts.infos && opts.infos.errors && opts.infos.errors.length > 0
  </script>
</highlight>

<expr-emp class='term emp'>
  <type data={data}/>
  this.mixin(Mixin.Data)
</expr-emp>

<var-expr class='term var'>
  <span class='token'>{opts.data.value0}</span>
  this.mixin(Mixin.Data)
</var-expr>

<expr-lambda class='term lam'>
  <div class='left'>
    <span class='token'>λ</span>
    <pattern each={d, i in params} data={d} renew={renewA(i)}/>
    <add-area renew={addArg}/>
    <span class='token'>→</span>
  </div>
  <expr data={expr} hole={true} right={true}/>

  <script>
    this.mixin(Mixin.Data)
    this.params = this.data.value0
    this.expr   = this.data.value1
    this.onrenew = d => this.renew(I.lambdaC(this.params)(d))
    this.renewA  = i => d => this.renew(d ? I.renewLambda(i)(d)(this.params)(this.expr)
                                          : I.deleteLambda(i)(this.params)(this.expr) )
    this.addArg  = d => this.renew(I.lambdaC(this.params.concat(d || I.pempty))(this.expr))
  </script>
</expr-lambda>

<app-expr class='term {opts.spine} {opts.outer?"outer":""}'>
  <expr data={data.value0} spine={opts.spine} left={true} renew={renewL}/>
  <expr data={data.value1} bracket={true} right={opts.outer} renew={renewR}/>
  <script>
    this.mixin(Mixin.Data)
    this.renewL = d => this.renew(I.appC(d)(this.data.value1))
    this.renewR = d => this.renew(I.appC(this.data.value0)(d))
  </script>
</app-expr>

<expr-oper class='term opv'>
  <div class='left'>
    <expr if={a} data={a} hole={true} left={true} renew={renewA}/>
    <hole if={!a} data={holes[0]} spine='opv' left={true} renew={renewA}/>
  </div>
  <oper-var data={o}/>
  <div class='right'>
    <expr if={b} data={b} hole={true} right={true} renew={renewB}/>
    <hole if={!b} data={holes[1]} spine='opv' right={true} renew={renewB}/>
  </div>

  <script>
    this.mixin(Mixin.Data)
    this.o = this.data.value0
    this.a = this.data.value1.value0
    this.b = this.data.value2.value0
    this.holes = I.arrowToArray(this.o.value1.value1); this.holes.pop()
    this.renewA  = a => this.renew(this.cons(this.o, a, this.b))
    this.renewB  = b => this.renew(this.cons(this.o, this.a, b))
    this.onrenew = o => this.renew(this.cons(o, this.a, this.b))
    this.cons = (o, a, b) => a ? b ? I.operC(o)(a)(b) : I.operCA(o)(a)
                               : b ? I.operCB(o)(b)   : I.operC0(o)
  </script>
</expr-oper>

<oper-var class='term opv'>
  <span class='token'>{expr.value0}</span>

  <infos show={hover} data={infos}/>
  <type-info show={hover} data={scheme}/>
  <highlight hover={hover} infos={infos}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.expr   = this.data.value0
    this.scheme = this.data.value1
    this.infos  = this.data.value2
  </script>
</oper-var>

<num-expr class='term num'>
  <handle/>
  <input-field data={data.value0}/>
  <!--<span class='token'>{opts.data.value0}</span>-->
  <script>
    this.mixin(Mixin.Data)
    this.onrenew = v => this.renew(I.numC(v))
  </script>
</num-expr>

<expr-if class='term ift'>
  <span class='token'>if</span>
  <expr data={c} hole={true}/>
  <span class='token'>then</span>
  <expr data={a} hole={true} renew={renewA}/>
  <span class='token'>else</span>
  <expr data={b} hole={true} right={true} renew={renewB}/>
  <script>
    this.mixin(Mixin.Data)
    this.c = this.data.value0
    this.a = this.data.value1
    this.b = this.data.value2
    this.onrenew = d => this.renew(I.ifC(d)(this.a)(this.b))
    this.renewA  = d => this.renew(I.ifC(this.c)(d)(this.b))
    this.renewB  = d => this.renew(I.ifC(this.c)(this.a)(d))
  </script>
</expr-if>

<expr-case class='term cas'>
  <div class='left'>
    <span class='token'>case</span>
    <expr data={a} hole={true}/>
    <span class='token'>of</span>
  </div>
  <div ref="list" class='list'>
    <case-alter each={d, i in as} data={d} renew={renewA(i)} remove={as1 && deleteA(i)}/>
  </div>
  <button onclick={addA} class='add'>+</button>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Sortable)
    this.name = 'alter'
    this.a   = this.data.value0
    this.as  = this.listData = this.data.value1
    this.as1 = this.as.length > 1
    this.onrenew = d => this.renew(I.caseC(d)(this.as))
    this.renewA  = i => d => this.renew(I.caseC(this.a)(I.renewI(i)(d)(this.as)))
    this.deleteA = i => d => this.renew(I.caseC(this.a)(I.deleteI(i)(this.as)))
    // this.renewP = i => d => this.renew(I.caseC(this.a)(I.renewFirsts(i)(d)(this.as)))
    // this.renewB = i => d => this.renew(I.caseC(this.a)(I.renewSeconds(i)(d)(this.as)))
    this.addA   = () => this.renew(I.caseC(this.a)(this.as.concat(I.aempty)))
    this.onsort = d => this.renew(I.caseC(this.a)(d))
  </script>
</expr-case>

<case-alter class={hover: hover}>
  <button if={opts.remove} onclick={opts.remove} class='remove'>×</button>
  <pattern data={p} outer={true} renew={renewP}/>
  <span class='token'>→</span>
  <expr data={b} outer={true}/>
  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.p = this.data.value0
    this.b = this.data.value1
  </script>
</case-alter>

// Pattern ---------------------------------------------------------------------

<pattern class={outer: opts.outer, right: opts.right}>
  <div ref='slot' class='slot'>
    <var-pattern if={cons == 'var'} data={expr}/>
  </div>

  <type-info show={hover} data={scheme}/>
  <highlight hover={hover}/>

  <script>
    this.mixin(Mixin.Data)
    this.mixin(Mixin.Selectable)
    this.mixin(Mixin.Clonable)

    this.name   = 'expr'
    this.expr   = this.data.value0
    this.cons   = I.econs(this.expr)
    this.scheme = this.data.value1

    this.onrenew = d => {
        this.renew(d && I.renewExpr(d)(this.data))
    }
  </script>
</pattern>

<var-pattern class='term var'>
  <handle remove={remove}/>
  <input-field data={data.value0}/>
  <!--<span class='token'>{data.value0}</span>-->
  <script>
    this.mixin(Mixin.Data)
    this.onrenew = s => this.renew(I.varC(s))
    this.remove  = () => this.renew(null)
  </script>
</var-pattern>

<input-field>
  <input ref='input' type='text' value={data} onchange={onchange} oninput={oninput}/>
  <script>
    this.mixin(Mixin.Data)
    this.on('mount', () => this.oninput())
    this.onchange = e => this.renew(this.refs.input.value)
    this.oninput  = e => this.refs.input.style.width = this.refs.input.value.length + 'ch'
  </script>
</input-field>
