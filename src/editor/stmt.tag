// Statement

<statement class='card'>
  <bind-stmt data={opts.data}/>
</statement>


<bind-stmt>
  <bind each={data in opts.data.value0} data={data}/>

  <script>
    console.log(opts.data);
  </script>
</bind-stmt>
